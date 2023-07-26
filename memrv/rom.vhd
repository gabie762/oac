library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity mem_rom is
 port (
 	  clock: in std_logic;
    we: in std_logic;
    address: in std_logic_vector(7 downto 0);
    dataout: out std_logic_vector(31 downto 0)
    );
end mem_rom;

architecture RTL of mem_rom is
  type rom_type is array (0 to (2**address'length)-1) of std_logic_vector(31 downto 0);
  
  impure function read_rom_data return rom_type is
    file rom_file: text open read_mode is "rom_data.txt";
    variable txt_line: line;
    variable rom_data: rom_type;
  begin
    for i in 0 to 255 loop
      readline(rom_file, txt_line);
      hread(txt_line, rom_data(i));
    end loop;
    return rom_data;
  end function;
  
  signal mem: rom_type:= read_rom_data;
  signal read_address : std_logic_vector(address'range);

begin
Read: process(clock) 
	begin
	if(rising_edge(clock)) then
      -- read_address <= address;
      dataout <= mem(to_integer(unsigned(address)));
  end if;
  end process;
end RTL;
