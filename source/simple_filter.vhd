library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity simple_filter is
  generic(
    IWL         :       natural := 16;              -- input word length
    CWL         :       natural := 16;              -- coefficient word length
    OWL         :       natural := 16;              -- output word length
    N           :       natural := 8                -- fiter order
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    sig_in      :   in  std_logic_vector(IWL-1 downto 0);
    sig_out     :   out std_logic_vector(OWL-1 downto 0) 
  );
end simple_filter;


architecture simple_filter_arch of simple_filter is
    type vector_array is array (natural range<>) of std_logic_vector;

component mac_mult is
  generic(
    IWL         :       natural;
    CWL         :       natural;
    OWL         :       natural
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    coef        :   in  std_logic_vector(IWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0)
  );
end component;

procedure push_front(signal queue : inout vector_array(0 to N-1)(IWL-1 downto 0);
                     signal val   : in    std_logic_vector(IWL-1 downto 0)) is
  begin
    for i in N-1 downto 1 loop
        queue(i) <= queue(i-1);
    end loop;

    queue(0) <= val;
end procedure;
    constant COEF : vector_array(0 to N-1)(CWL-1 downto 0) := ( others => std_logic_vector(
                    to_signed(integer(ceil((0.5**3) * 2.0**(CWL-1))), 
                              CWL)));
        -- 0 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**1) * 2.0**(CWL-1))), 
        --                       CWL)),
        -- 1 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**2) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 2 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**3) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 3 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**4) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 4 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**5) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 5 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**6) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 6 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**7) * 2.0**(CWL-1))), 
        --                         CWL)),
        -- 7 => std_logic_vector(
        --             to_signed(integer(ceil((0.5**8) * 2.0**(CWL-1))), 
        --                         CWL)));

    signal mac_coef     : std_logic_vector(CWL-1 downto 0);
    signal mac_sig      : std_logic_vector(IWL-1 downto 0);
    signal mac_out      : std_logic_vector(OWL-1 downto 0);

    signal sig_buf      : vector_array(0 to N-1)(IWL-1 downto 0);
    signal state        : integer range 0 to N;
  begin
    mac0: mac_mult
        generic map(IWL, CWL, OWL)
        port map(clk, strobe, reset, mac_coef, mac_sig, mac_out);
    sig_out  <= mac_out;
    mac_sig  <= sig_buf(state);
    mac_coef <= COEF(state);

    filt_main_proc : process(clk, reset)
      begin
        if (reset = '1') then
            state   <= 0;
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            state <= state + 1 when state < N-1 else state;

            if (strobe = '1') then
                state <= 0;
                push_front(sig_buf, sig_in);
            end if;
        end if;
    end process;
end simple_filter_arch;