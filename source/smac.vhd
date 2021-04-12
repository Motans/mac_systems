library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity smac is
  generic(
    IWL         :       natural := 16;                      -- input word length
    CWL         :       natural := 16;                      -- coefficient word length
    OWL         :       natural := 16                       -- output word length
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    coef        :   in  std_logic_vector(CWL-1 downto 0);
    sig0        :   in  std_logic_vector(IWL-1 downto 0);
    sig1        :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end smac;


architecture smac_arch of smac is
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
    coef        :   in  std_logic_vector(CWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end component;
    signal sum : std_logic_vector(IWL-1 downto 0);
  begin
    sum <= std_logic_vector(
        signed(sig0) + signed(sig1));

    mac0: mac_mult
        generic map(IWL, CWL, OWL)
        port map(clk, strobe, reset, coef, sum, word_out);
end smac_arch;