library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity xREGS is
  generic (WSIZE: natural := 32);
  port(
    clk, we, rst: in std_logic;
    rs1, rs2, rd: in std_logic_vector(4 downto 0);
    data_in: in std_logic_vector(WSIZE-1 downto 0);
    data_out1: out std_logic_vector(WSIZE-1 downto 0);
    data_out2: out std_logic_vector(WSIZE-1 downto 0)
    );
end entity xREGS;

architecture behaviour of xREGS is
  type reg_array is array(natural range <>) of std_logic_vector(WSIZE-1 downto 0);
  signal breg: reg_array(31 downto 0);
  signal clock, reset, wren: std_logic;
  signal reg1, reg2, regd: std_logic_vector(4 downto 0);
  signal wrdata: std_logic_vector(WSIZE-1 downto 0);
  signal rout1, rout2: std_logic_vector(WSIZE-1 downto 0);
  
begin
  process(clock, reset) is
    begin
    if(reset = '1') then
      breg <= (others => (others => '0'));
    end if;
    
    if (rising_edge(clk)) then
      if(wren = '1') then
        if(rd /= "00000") then
          breg(to_integer(unsigned(rd))) <= data_in;
        end if;
      end if;
      rout1 <= breg(to_integer(unsigned(reg1)));
      data_out1 <= rout1;
      rout2 <= breg(to_integer(unsigned(reg2)));
      data_out2 <= rout2;
    end if;
  end process;
  clock <= clk;
  wren <= we;
end behaviour;