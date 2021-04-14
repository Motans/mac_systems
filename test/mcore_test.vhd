library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity mcore_test is
end mcore_test;


architecture mcore_test_arch of mcore_test is
procedure clk_event(signal   clk    : out std_logic; 
                    constant FREQ   :     real; 
                    constant STIME  :     natural) is
    constant PERIOD    : time := 1 sec / FREQ;        -- Full period
    constant HIGH_TIME : time := PERIOD / 2;          -- High time
    constant LOW_TIME  : time := PERIOD - HIGH_TIME;  -- Low time; always >= HIGH_TIME
  begin
    for i in 0 to STIME-1 loop
        clk <= '1';
        wait for HIGH_TIME;
        clk <= '0';
        wait for LOW_TIME;
    end loop;
end procedure;

procedure strobe_event(signal   strobe      : out   std_logic;
                       constant FREQ        :       real;
                       constant DURATION    :       time;
                       constant STIME       :       natural) is
    constant PERIOD    : time := 1 sec / FREQ;          -- Full period
    constant HIGH_TIME : time := DURATION;              -- High time
    constant LOW_TIME  : time := PERIOD - HIGH_TIME;    -- Low time; always >= HIGH_TIME
  begin
    for i in 0 to STIME-1 loop
        strobe <= '1';
        wait for HIGH_TIME;
        strobe <= '0';
        wait for LOW_TIME;
    end loop;
end procedure;

component mcore_filt is
  generic(
    IWL         :       natural;
    CWL         :       natural;
    OWL         :       natural;
    N           :       natural;
    cores       :       natural;
    symmetric   :       boolean);
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    sig_in      :   in  std_logic_vector(IWL-1 downto 0);
    sig_out     :   out std_logic_vector(OWL-1 downto 0));
end component;

    constant    IWL         :   integer := 16;
    constant    CWL         :   integer := 16;
    constant    OWL         :   integer := 16;
    constant    STIME       :   natural := 2**10;
    constant    N           :   natural := 80;
    constant    N_2         :   natural := natural(ceil(real(N) / 2.0));
    constant    CORES       :   natural := 7;
    constant    FREQ        :   real    := 1000.0;
    constant    TK          :   integer := integer(ceil(real(N) / real(CORES)));

    signal      clk         :   std_logic;
    signal      clk_filt    :   std_logic;
    signal      strobe      :   std_logic;
    signal      reset       :   std_logic;

    signal      sig         :   std_logic_vector(IWL-1 downto 0);
    signal      out_res     :   std_logic_vector(OWL-1 downto 0);

  begin
    clk_event(clk, FREQ, TK*STIME);
    strobe_event(strobe, FREQ/real(TK), 0.5 sec / FREQ, STIME);
    reset <= '1', '0' after (real(TK) * sec / FREQ);
    clk_filt <= clk;

    filt0: mcore_filt
        generic map(IWL, CWL, OWL, N, CORES, false)
        port map(clk_filt, strobe, reset, sig, out_res);

    event: process(clk, reset)
        variable i : integer;
      begin
        if (reset = '1') then
            i := 0;
        elsif(clk'event and clk = '1') then
            if (strobe = '1') then
                if (i = 0) then
                    sig <= std_logic_vector(
                        to_signed(2**(IWL-1) - 1, CWL));
                else
                    sig <= (others => '0');
                end if;
                i := i + 1;
            end if;
        end if;
    end process;
end mcore_test_arch;