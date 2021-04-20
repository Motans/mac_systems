library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity mac_mult is
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
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0) 
  );
end mac_mult;


architecture mac_mult_arch of mac_mult is
    constant AWL : natural := IWL + CWL;
  begin
    mac_main_pr : process(clk, reset)
        variable acc        : signed(AWL-1 downto 0);
        variable sat_res    : signed(OWL   downto 0);
        variable res        : signed(OWL-1 downto 0);
      begin
        if (reset = '1') then
            word_out <= (others => '0');
            acc      := (others => '0');
        elsif (clk'event and clk = '1') then
            acc := acc + signed(coef) * signed(sig);

            if (strobe = '1') then
                res     := (others => '0');
                sat_res := acc(AWL-1 downto AWL - OWL - 1);

                if (sat_res(OWL) /= sat_res(OWL-1)) then                          --Saturate
                    res(OWL-1)          := sat_res(OWL);
                    res(OWL-2 downto 0) := (others => sat_res(OWL-1));
                else
                    res := sat_res(OWL-1 downto 0);
                end if;
    
                word_out <= std_logic_vector(res);
                acc      := (others => '0');
            end if;
        end if;
    end process;
end mac_mult_arch;