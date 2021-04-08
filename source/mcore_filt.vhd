library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity mcore_filt is
  generic(
    IWL         :       natural := 16;              -- input word length
    CWL         :       natural := 16;              -- coefficient word length
    OWL         :       natural := 16;              -- output word length
    N           :       natural := 8;               -- fiter order
    cores       :       natural := 2;               -- number of cores
    symmetric   :       boolean
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    sig_in      :   in  std_logic_vector(IWL-1 downto 0);
    sig_out     :   out std_logic_vector(OWL-1 downto 0) 
  );
end mcore_filt;


architecture mcore_filt_arch of mcore_filt is
    type vector_array_IWL is array (natural range<>) of std_logic_vector(IWL-1 downto 0);
    type vector_array_CWL is array (natural range<>) of std_logic_vector(CWL-1 downto 0);
    type vector_array_OWL is array (natural range<>) of std_logic_vector(OWL-1 downto 0);

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

procedure push_front(signal queue : inout vector_array_IWL(0 to N-1);
                     signal val   : in    std_logic_vector(IWL-1 downto 0)) is
  begin
    for i in N-1 downto 1 loop
        queue(i) <= queue(i-1);
    end loop;

    queue(0) <= val;
end procedure;

    signal mac_coef : vector_array_CWL(0 to cores-1);
    signal mac_sig  : vector_array_IWL(0 to cores-1);
    signal mac_out  : vector_array_OWL(0 to cores-1);
  begin
    mac_gen : for k in 0 to cores-1 generate
        mac0: mac_mult
            generic map(IWL, CWL, OWL)
            port map(clk, strobe, reset, mac_coef(k), mac_sig(k), mac_out(k));
    end generate;

symm : if symmetric generate
    constant SIZE_2     : natural := natural(ceil(real(N) / 2.0));
    constant BUF_SIZE   : natural := natural(ceil(real(SIZE_2) / real(cores))) * cores;
    constant COEF       : vector_array_CWL(0 to BUF_SIZE-1) := (
        0   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.000507108001499 * 2.0**(CWL-1))), 
                              CWL)),
        1   =>  std_logic_vector(
                    to_signed(integer(ceil(0.000605885435266 * 2.0**(CWL-1))), 
                              CWL)),
        2   =>  std_logic_vector(
                    to_signed(integer(ceil(0.002234629082015 * 2.0**(CWL-1))), 
                              CWL)),
        3   =>  std_logic_vector(
                    to_signed(integer(ceil(0.004132057443708 * 2.0**(CWL-1))), 
                              CWL)),
        4   =>  std_logic_vector(
                    to_signed(integer(ceil(0.004989659835467 * 2.0**(CWL-1))), 
                              CWL)),
        5   =>  std_logic_vector(
                    to_signed(integer(ceil(0.002776106372470 * 2.0**(CWL-1))), 
                              CWL)),
        6   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.003932564866231 * 2.0**(CWL-1))), 
                              CWL)),
        7   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.014233556164278 * 2.0**(CWL-1))), 
                              CWL)),
        8   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.023880160004658 * 2.0**(CWL-1))), 
                              CWL)),
        9   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.025932499335325 * 2.0**(CWL-1))), 
                              CWL)),
        10  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.013265496183571 * 2.0**(CWL-1))), 
                              CWL)),
        11  =>  std_logic_vector(
                    to_signed(integer(ceil(0.017937981562636 * 2.0**(CWL-1))), 
                              CWL)),
        12  =>  std_logic_vector(
                    to_signed(integer(ceil(0.065337905676027 * 2.0**(CWL-1))), 
                              CWL)),
        13  =>  std_logic_vector(
                    to_signed(integer(ceil(0.119829195881356 * 2.0**(CWL-1))), 
                              CWL)),
        14  =>  std_logic_vector(
                    to_signed(integer(ceil(0.167867384812280 * 2.0**(CWL-1))), 
                              CWL)),
        15  =>  std_logic_vector(
                    to_signed(integer(ceil(0.196040578454338 * 2.0**(CWL-1))), 
                              CWL)),
        others  => (others => '0'));

    signal sig_buf  : vector_array_IWL(0 to N-1);
    signal buf_2    : vector_array_IWL(0 to BUF_SIZE-1);
    signal i        : integer range 0 to BUF_SIZE-1;
    
  begin
    even : if (N rem 2 = 0) generate
        even_sum : for j in 0 to SIZE_2-1 generate
            buf_2(j) <= std_logic_vector(
                signed(sig_buf(j)) + signed(sig_buf(N - j - 1)));
        end generate;
    end generate;

    odd : if (N rem 2 = 1) generate
        odd_sum : for j in 0 to SIZE_2-2 generate
            buf_2(j) <= std_logic_vector(
                signed(sig_buf(j)) + signed(sig_buf(N - j - 1)));
        end generate;
        buf_2(SIZE_2-1) <= sig_buf(SIZE_2-1);
    end generate;

    gen_sig : for k in 0 to cores-1 generate
        mac_sig(k)  <= buf_2(i + k);
        mac_coef(k) <= COEF(i + k);
    end generate;

    symm_main_proc : process(clk, reset)
        variable acc : signed(OWL-1 downto 0);
      begin
        if (reset = '1') then
            i   <= 0;
            acc := (others => '0');
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            if (strobe /= '1') then 
                i <= i + cores;
            else
                i <= 0;

                for j in 0 to cores-1 loop
                    acc := acc + signed(mac_out(j));
                end loop;

                sig_out <= std_logic_vector(acc);
                acc     := (others => '0');
                push_front(sig_buf, sig_in);
            end if;
        end if;
    end process;
end generate;

not_symm : if not symmetric generate
    constant BUF_SIZE   : natural := natural(ceil(real(N) / real(cores))) * cores;
    constant COEF       : vector_array_CWL(0 to BUF_SIZE-1) := (
        0   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.000507108001499 * 2.0**(CWL-1))), 
                              CWL)),
        1   =>  std_logic_vector(
                    to_signed(integer(ceil(0.000605885435266 * 2.0**(CWL-1))), 
                              CWL)),
        2   =>  std_logic_vector(
                    to_signed(integer(ceil(0.002234629082015 * 2.0**(CWL-1))), 
                              CWL)),
        3   =>  std_logic_vector(
                    to_signed(integer(ceil(0.004132057443708 * 2.0**(CWL-1))), 
                              CWL)),
        4   =>  std_logic_vector(
                    to_signed(integer(ceil(0.004989659835467 * 2.0**(CWL-1))), 
                              CWL)),
        5   =>  std_logic_vector(
                    to_signed(integer(ceil(0.002776106372470 * 2.0**(CWL-1))), 
                              CWL)),
        6   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.003932564866231 * 2.0**(CWL-1))), 
                              CWL)),
        7   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.014233556164278 * 2.0**(CWL-1))), 
                              CWL)),
        8   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.023880160004658 * 2.0**(CWL-1))), 
                              CWL)),
        9   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.025932499335325 * 2.0**(CWL-1))), 
                              CWL)),
        10  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.013265496183571 * 2.0**(CWL-1))), 
                              CWL)),
        11  =>  std_logic_vector(
                    to_signed(integer(ceil(0.017937981562636 * 2.0**(CWL-1))), 
                              CWL)),
        12  =>  std_logic_vector(
                    to_signed(integer(ceil(0.065337905676027 * 2.0**(CWL-1))), 
                              CWL)),
        13  =>  std_logic_vector(
                    to_signed(integer(ceil(0.119829195881356 * 2.0**(CWL-1))), 
                              CWL)),
        14  =>  std_logic_vector(
                    to_signed(integer(ceil(0.167867384812280 * 2.0**(CWL-1))), 
                              CWL)),
        15  =>  std_logic_vector(
                    to_signed(integer(ceil(0.196040578454338 * 2.0**(CWL-1))), 
                              CWL)),
        16  =>  std_logic_vector(
                    to_signed(integer(ceil(0.196040578454338 * 2.0**(CWL-1))), 
                              CWL)),
        17  =>  std_logic_vector(
                    to_signed(integer(ceil(0.167867384812280 * 2.0**(CWL-1))), 
                              CWL)),
        18  =>  std_logic_vector(
                    to_signed(integer(ceil(0.119829195881356 * 2.0**(CWL-1))), 
                              CWL)),
        19  =>  std_logic_vector(
                    to_signed(integer(ceil(0.065337905676027 * 2.0**(CWL-1))), 
                              CWL)),
        20  =>  std_logic_vector(
                    to_signed(integer(ceil(0.017937981562636 * 2.0**(CWL-1))), 
                              CWL)),
        21   =>  std_logic_vector(
                    to_signed(integer(ceil(-0.013265496183571 * 2.0**(CWL-1))), 
                              CWL)),
        22  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.025932499335325 * 2.0**(CWL-1))), 
                              CWL)),
        23  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.023880160004658 * 2.0**(CWL-1))), 
                              CWL)),
        24  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.014233556164278 * 2.0**(CWL-1))), 
                              CWL)),
        25  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.003932564866231 * 2.0**(CWL-1))), 
                              CWL)),
        26  =>  std_logic_vector(
                    to_signed(integer(ceil(0.002776106372470 * 2.0**(CWL-1))), 
                              CWL)),
        27  =>  std_logic_vector(
                    to_signed(integer(ceil(0.004989659835467 * 2.0**(CWL-1))), 
                              CWL)),
        28  =>  std_logic_vector(
                    to_signed(integer(ceil(0.004132057443708 * 2.0**(CWL-1))), 
                              CWL)),
        29  =>  std_logic_vector(
                    to_signed(integer(ceil(0.002234629082015 * 2.0**(CWL-1))), 
                              CWL)),
        30  =>  std_logic_vector(
                    to_signed(integer(ceil(0.000605885435266 * 2.0**(CWL-1))), 
                              CWL)),
        31  =>  std_logic_vector(
                    to_signed(integer(ceil(-0.000507108001499 * 2.0**(CWL-1))), 
                              CWL)),        
        others  => (others => '0'));

    signal sig_buf  : vector_array_IWL(0 to BUF_SIZE-1);
    signal i        : integer range 0 to BUF_SIZE-1;
  begin
    gen_sig : for k in 0 to cores-1 generate
        mac_sig(k)  <= sig_buf(i + k);
        mac_coef(k) <= COEF(i + k);
    end generate;

    not_symm_main_proc : process(clk, reset)
        variable acc : signed(OWL-1 downto 0);
      begin
        if (reset = '1') then
            i       <= 0;
            acc     := (others => '0');
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            if (strobe /= '1') then
                i <= i + cores;
            else
                i <= 0;

                for j in 0 to cores-1 loop
                    acc := acc + signed(mac_out(j));
                end loop;

                sig_out <= std_logic_vector(acc);
                acc     := (others => '0');
                push_front(sig_buf, sig_in);
            end if;
        end if;
    end process;
end generate;
end mcore_filt_arch;