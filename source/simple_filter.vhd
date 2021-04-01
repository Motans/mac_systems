library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity simple_filter is
  generic(
    IWL         :       natural := 16;                      -- input word length
    CWL         :       natural := 16;                      -- coefficient word length
    OWL         :       natural := 16                       -- output word length
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    coef        :   in  std_logic_vector(IWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end simple_filter;


architecture simple_filter_arch of simple_filter is
component mac_mult is
  generic(
    IWL         :       natural := 16;                      -- input word length
    CWL         :       natural := 16;                      -- coefficient word length
    OWL         :       natural := 16                       -- output word length
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    coef        :   in  std_logic_vector(IWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end component;

  begin
    filt_main_pr : process(clk)
      begin
        if (clk'event and clk = '1') then
            if (strobe = '1') then
            end if;
            
        end if;
    end process;
end simple_filter_arch;