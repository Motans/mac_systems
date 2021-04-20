library ieee;
library work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package coef_data is
    type icoef_arr is array(natural range<>) of integer;
    constant COEF : icoef_arr := (
            14,     23,     27,     20,     -6,    -42,    -72,    -81,    -52,      7,
            79,    122,    105,     23,   -101,   -199,   -216,   -117,     73,    266,
           358,    272,     14,   -314,   -547,   -527,   -212,    302,    777,    938,
           617,   -154,  -1081,  -1703,  -1572,   -435,   1606,   4093,   6338,   7668,
          7668,   6338,   4093,   1606,   -435,  -1572,  -1703,  -1081,   -154,    617,
           938,    777,    302,   -212,   -527,   -547,   -314,     14,    272,    358,
           266,     73,   -117,   -216,   -199,   -101,     23,    105,    122,     79,
             7,    -52,    -81,    -72,    -42,     -6,     20,     27,     23,     14
    );
end package;


library ieee;
library work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.coef_data.all;


entity mcore_filt is
  generic(
    IWL         :       natural;                        -- input word length
    CWL         :       natural;                        -- coefficient word length
    OWL         :       natural;                        -- output word length
    N           :       natural;                        -- fiter order
    cores       :       natural;                        -- number of cores
    symmetric   :       boolean);                       -- symmetric filter or else
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    sig_in      :   in  std_logic_vector(IWL-1 downto 0);
    sig_out     :   out std_logic_vector(OWL-1 downto 0));
end mcore_filt;


architecture mcore_filt_arch of mcore_filt is
    type vector_array_IWL is array (natural range<>) of std_logic_vector(IWL-1 downto 0);
    type vector_array_CWL is array (natural range<>) of std_logic_vector(CWL-1 downto 0);
    type vector_array_OWL is array (natural range<>) of std_logic_vector(OWL-1 downto 0);
    

component mac_mult is
  generic(
    IWL         :       natural;
    CWL         :       natural;
    OWL         :       natural);
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    coef        :   in  std_logic_vector(IWL-1 downto 0);
    sig         :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0));
end component;

component smac is
  generic(
    IWL         :       natural;
    CWL         :       natural;
    OWL         :       natural);
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    coef        :   in  std_logic_vector(CWL-1 downto 0);
    sig0        :   in  std_logic_vector(IWL-1 downto 0);
    sig1        :   in  std_logic_vector(IWL-1 downto 0);
    word_out    :   out std_logic_vector(OWL-1 downto 0));
end component;

procedure push_front(signal queue : inout vector_array_IWL(0 to N-1);
                     signal val   : in    std_logic_vector(IWL-1 downto 0)) is
  begin
    for i in N-1 downto 1 loop
        queue(i) <= queue(i-1);
    end loop;

    queue(0) <= val;
end procedure;

    signal mac_coef : vector_array_CWL(0 to cores-1);
    signal mac_out  : vector_array_OWL(0 to cores-1);
    signal out_buf  : vector_array_OWL(0 to cores-1);
    signal sig_buf  : vector_array_IWL(0 to N-1);
  begin
    out_buf(0) <= mac_out(0);
    out_gen : for j in 1 to cores-1 generate
        out_buf(j) <= std_logic_vector(
            signed(out_buf(j-1)) + signed(mac_out(j)));
    end generate;

symm : if symmetric generate
    constant    N_2         : natural := natural(ceil(real(N) / 2.0));
    constant    CONV_SIZE   : natural := natural(ceil(real(N_2) / real(cores)));
    constant    TAIL        : natural := N_2 rem CONV_SIZE;

    type conv_IWL is array (natural range<>) of vector_array_IWL(0 to CONV_SIZE-1);
    type conv_CWL is array (natural range<>) of vector_array_CWL(0 to CONV_SIZE-1);
       
    signal      mac_sig0    : vector_array_IWL(0 to cores-1);
    signal      mac_sig1    : vector_array_IWL(0 to cores-1);
    signal      conv_coef   : conv_CWL(0 to cores-1);
    signal      conv_buf0   : conv_IWL(0 to cores-1);
    signal      conv_buf1   : conv_IWL(0 to cores-1);
    signal      i           : integer range    0 to CONV_SIZE-1;
  begin

    conv_gen0: for j in 0 to cores-2 generate
        conv_gen1: for k in 0 to CONV_SIZE-1 generate
            conv_coef(j)(k) <= std_logic_vector(
                to_signed(COEF(CONV_SIZE*j + k), CWL));

            conv_buf0(j)(k) <= sig_buf(CONV_SIZE*j + k);
            conv_buf1(j)(k) <= sig_buf((N-1) - (CONV_SIZE*j + k));
        end generate;
    end generate;

    tail_gen0 : for j in 0 to TAIL-2 generate
        conv_coef(cores-1)(j) <= std_logic_vector(
                to_signed(COEF((cores-1)*CONV_SIZE + j), CWL));

        conv_buf0(cores-1)(j) <= sig_buf((cores-1)*CONV_SIZE + j);
        conv_buf1(cores-1)(j) <= sig_buf((N-1) - ((cores-1)*CONV_SIZE + j));
    end generate;

    odd : if (1 = N rem 2) generate
        conv_coef(cores-1)(TAIL-1) <= std_logic_vector(
                to_signed(COEF(N_2-1), CWL));
        conv_buf0(cores-1)(TAIL-1) <= sig_buf(N_2-1);
        conv_buf1(cores-1)(TAIL-1) <= (others => '0');
    end generate;

    even : if (0 = N rem 2) generate
        conv_coef(cores-1)(TAIL-1) <= std_logic_vector(
                to_signed(COEF(N_2-1), CWL));
        conv_buf0(cores-1)(TAIL-1) <= sig_buf(N_2-1);
        conv_buf1(cores-1)(TAIL-1) <= sig_buf(N_2);
    end generate;

    conv_coef(cores-1)(TAIL to CONV_SIZE-1) <= (others => (others => '0'));
    conv_buf0(cores-1)(TAIL to CONV_SIZE-1) <= (others => (others => '0'));
    conv_buf1(cores-1)(TAIL to CONV_SIZE-1) <= (others => (others => '0'));

    mac_gen : for k in 0 to cores-1 generate
        mac0: smac
            generic map(IWL, CWL, OWL)
            port map(clk, strobe, reset, mac_coef(k),
                     mac_sig0(k), mac_sig1(k), mac_out(k));

        mac_sig0(k) <= conv_buf0(k)(i);
        mac_sig1(k) <= conv_buf1(k)(i);
        mac_coef(k) <= conv_coef(k)(i);
    end generate;

    symm_main_proc : process(clk, reset)
      begin
        if (reset = '1') then
            i       <= 0;
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            if (strobe /= '1') then 
                i <= i + 1;
            else
                i       <= 0;
                push_front(sig_buf, sig_in);
                sig_out <= out_buf(cores-1);
            end if;
        end if;
    end process;
end generate;

not_symm : if not symmetric generate
    constant    CONV_SIZE   : natural := natural(ceil(real(N) / real(cores)));
    constant    TAIL        : natural := N rem CONV_SIZE;

    type conv_IWL is array (natural range<>) of vector_array_IWL(0 to CONV_SIZE-1);
    type conv_CWL is array (natural range<>) of vector_array_CWL(0 to CONV_SIZE-1);
    
    signal      conv_coef   : conv_CWL(0 to cores-1);
    signal      conv_buf    : conv_IWL(0 to cores-1);
    signal      mac_sig     : vector_array_IWL(0 to cores-1);
    signal      i           : integer range    0 to CONV_SIZE-1;
  begin
    conv_gen0: for j in 0 to cores-2 generate
        conv_gen1: for k in 0 to CONV_SIZE-1 generate
            conv_coef(j)(k) <= std_logic_vector(
                to_signed(COEF(CONV_SIZE*j + k), CWL));

            conv_buf(j)(k) <= sig_buf(CONV_SIZE*j + k);
        end generate;
    end generate;
    tail_gen0 : for j in 0 to TAIL-1 generate
        conv_coef(cores-1)(j) <= std_logic_vector(
                to_signed(COEF((cores-1)*CONV_SIZE + j), CWL));

        conv_buf (cores-1)(j) <= sig_buf((cores-1)*CONV_SIZE + j);
    end generate;

    conv_coef(cores-1)(TAIL to CONV_SIZE-1) <= (others => (others => '0'));
    conv_buf (cores-1)(TAIL to CONV_SIZE-1) <= (others => (others => '0'));

    mac_gen : for k in 0 to cores-1 generate
        mac0: mac_mult
            generic map(IWL, CWL, OWL)
            port map(clk, strobe, reset, 
                     mac_coef(k), mac_sig(k), mac_out(k));

        mac_sig(k)  <= conv_buf(k)(i);
        mac_coef(k) <= conv_coef(k)(i);
    end generate;

    not_symm_main_proc : process(clk, reset)
      begin
        if (reset = '1') then
            i       <= 0;
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            if (strobe /= '1') then
                i <= i + 1;
            else
                i       <= 0;
                push_front(sig_buf, sig_in);
                sig_out <= out_buf(cores-1);
            end if;
        end if;
    end process;
end generate;
end mcore_filt_arch;