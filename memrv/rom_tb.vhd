library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library std;
use std.textio.all;

entity mem_rom_tb is
end mem_rom_tb;

architecture Behavioral of mem_rom_tb is
  signal clock: std_logic;
  signal we: std_logic;
  signal address: std_logic_vector(7 downto 0);
  signal dataout: std_logic_vector(31 downto 0);

  component mem_rom is
    port (
      clock: in std_logic;
      we: in std_logic;
      address: in std_logic_vector(7 downto 0);
      dataout: out std_logic_vector);
  end component;

  type rom_type is array (0 to 255) of std_logic_vector(31 downto 0);
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
  
  signal rom: rom_type:= read_rom_data;
  --constant rom: rom_type := read_rom_data("rom_data.txt");

begin

  uut: mem_rom
    port map (
      clock => clock,
      we => we,
      address => address,
      dataout => dataout
    );

  clk: process
  begin
    clock <= '0';
    wait for 5 ns;
    clock <= '1';
    wait for 5 ns;
  end process;

  estimulos: process
  begin
    -- Escrever data p/ rom
    --we <= '1';
    --for i in 0 to 255 loop
     -- address <= std_logic_vector(to_unsigned(i, 8));
      --wait for 10 ns;
    --end loop;
    we <= '0';

    -- Ler data da ROM
    for i in 0 to 255 loop
      address <= std_logic_vector(to_unsigned(i, 8));
      dataout <= rom(to_integer(unsigned(address)));
      wait for 10 ns;
      assert dataout = rom(i)
        report "Unexpected data at address " & integer'image(i)
        severity error;
    end loop;

    wait;
  end process;

end Behavioral;
