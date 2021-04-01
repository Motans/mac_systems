library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_mac is
end test_mac;


architecture test_mac_arch of test_mac is
procedure clk_event(signal clk: out std_logic; 
                    constant FREQ: real; 
                    constant N: natural) is
    constant PERIOD    : time := 1 sec / FREQ;        -- Full period
    constant HIGH_TIME : time := PERIOD / 2;          -- High time
    constant LOW_TIME  : time := PERIOD - HIGH_TIME;  -- Low time; always >= HIGH_TIME
  begin
    for i in 0 to N-1 loop
        clk <= '1';
        wait for HIGH_TIME;
        clk <= '0';
        wait for LOW_TIME;
    end loop;
end procedure;

procedure strobe_event(signal strobe: out std_logic;
                       constant FREQ: real;
                       constant DURATION : time;
                       constant N: natural) is
    constant PERIOD    : time := 1 sec / FREQ;          -- Full period
    constant HIGH_TIME : time := DURATION;              -- High time
    constant LOW_TIME  : time := PERIOD - HIGH_TIME;    -- Low time; always >= HIGH_TIME
  begin
    for i in 0 to N-1 loop
        strobe <= '1';
        wait for HIGH_TIME;
        strobe <= '0';
        wait for LOW_TIME;
    end loop;
end procedure;

component mac_mult is
  generic(
    IWL         :       natural;                      -- input word length
    CWL         :       natural;                      -- coefficient word length
    OWL         :       natural                      -- output word length
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    coef        :   in  std_logic_vector(IWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end component;

    constant    IWL             :   integer := 16;
    constant    CWL             :   integer := 16;
    constant    OWL             :   integer := 16;
    constant    N               :   natural := 2**10;
    constant    FREQ            :   real    := 1000.0;
    
    signal      clk             :   std_logic;
    signal      clk_mac         :   std_logic;
    signal      strobe          :   std_logic;

    signal      coef            :   std_logic_vector(CWL-1 downto 0);
    signal      sig             :   std_logic_vector(IWL-1 downto 0);
    signal      out_res         :   std_logic_vector(OWL-1 downto 0);

  begin
    clk_event(clk, FREQ, 2*N);
    strobe_event(strobe, FREQ/2.0, 0.5 sec / FREQ, N);
    
    clk_mac <= clk;
    mac0: mac_mult
        generic map(IWL, CWL, OWL)
        port map(clk_mac, strobe, coef, sig, out_res);
	
    event: process(clk)
        variable i : integer := 0;
      begin
        if(clk'event and clk = '1') then
            coef <= std_logic_vector(
                to_signed(2**(CWL-1) / (i+2), CWL));
            sig <= std_logic_vector(
                to_signed(2**(IWL-1) / (i+2), CWL));
            i := i + 1;
        end if;
    end process;
end test_mac_arch;