library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity symmetric_filter is
  generic(
    IWL         :       natural := 16;              -- input word length
    CWL         :       natural := 16;              -- coefficient word length
    OWL         :       natural := 16;              -- output word length
    N           :       natural := 8;               -- fiter order
    symmetric   :       boolean
  );
  port(
    clk         :   in  std_logic;
    strobe      :   in  std_logic;
    reset       :   in  std_logic;
    sig_in      :   in  std_logic_vector(IWL-1 downto 0);
    sig_out     :   out std_logic_vector(OWL-1 downto 0) 
  );
end symmetric_filter;


architecture symmetric_filter_arch of symmetric_filter is
    type vector_array_IWL is array (natural range<>) of std_logic_vector(IWL-1 downto 0);
    type vector_array_CWL is array (natural range<>) of std_logic_vector(CWL-1 downto 0);

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

function ret_val(signal queue : vector_array_IWL(0 to N-1);
                 signal i     : integer range 0 to N)
    return std_logic_vector is
  begin
    if (i = N - 1 - i) then
        return queue(i);
    else
        return std_logic_vector(
            signed(queue(i)) + signed(queue(N - i - 1)));
    end if;
end function;

    constant COEF : vector_array_CWL(0 to N-1) := 
        (others => std_logic_vector(
                    to_signed(integer(ceil(0.125 * 2.0**(CWL-1))), 
                              CWL)));

    signal mac_clk      : std_logic;

    signal mac_coef     : std_logic_vector(CWL-1 downto 0);
    signal mac_sig      : std_logic_vector(IWL-1 downto 0);
    signal mac_out      : std_logic_vector(OWL-1 downto 0);

    signal sig_buf      : vector_array_IWL(0 to N-1);
    signal i            : integer range 0 to N;
  begin
    mac0: mac_mult
        generic map(IWL, CWL, OWL)
        port map(clk, strobe, reset, mac_coef, mac_sig, mac_out);

symm : if symmetric generate
    constant size_2 : integer := integer(ceil(real(N) / 2.0));
    signal   buf_2  : vector_array_IWL(0 to size_2-1);
  begin
    even : if (N rem 2 = 0) generate
        even_sum : for j in 0 to size_2-1 generate
            buf_2(j) <= std_logic_vector(
                signed(sig_buf(i)) + signed(sig_buf(N - i - 1)));
        end generate;
    end generate;

    odd : if (N rem 2 = 1) generate
        odd_sum : for j in 0 to size_2-2 generate
            buf_2(j) <= std_logic_vector(
                signed(sig_buf(i)) + signed(sig_buf(N - i - 1)));
        end generate;
        buf_2(size_2-1) <= sig_buf(size_2-1);
    end generate;

    sig_out  <= mac_out;
    mac_sig  <= buf_2(i);
    mac_coef <= COEF(i); 

    symm_main_proc : process(clk, reset)
      begin
        if (reset = '1') then
            i   <= 0;
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            i <= i + 1;-- when i < integer(ceil(real(N) / 2.0)) else i;

            if (strobe = '1') then
                i <= 0;
                push_front(sig_buf, sig_in);
            end if;
        end if;
    end process;
end generate;

not_symm : if not symmetric generate
    sig_out  <= mac_out;
    mac_sig  <= sig_buf(i);
    mac_coef <= COEF(i);

    not_symm_main_proc : process(clk, reset)
      begin
        if (reset = '1') then
            i       <= 0;
            sig_buf <= (others => (others => '0'));
        elsif (clk'event and clk = '1') then
            i <= i + 1;-- when i < N-1 else i;

            if (strobe = '1') then
                i <= 0;
                push_front(sig_buf, sig_in);
            end if;
        end if;
    end process;
end generate;
end symmetric_filter_arch;