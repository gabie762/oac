library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library std;
use std.textio.all;

entity mem_ram_tb is
end mem_ram_tb;

architecture Behavioral of mem_ram_tb is
  signal clock: std_logic;
  signal we: std_logic;
  signal address: std_logic_vector(7 downto 0);
  signal datain: std_logic_vector(31 downto 0);
  signal dataout: std_logic_vector(31 downto 0);

  component mem_ram is
    port (
      clock: in std_logic;
      we: in std_logic;
      address: in std_logic_vector(7 downto 0);
      datain: in std_logic_vector(31 downto 0);
      dataout: out std_logic_vector(31 downto 0)
      );
  end component;

  type ram_type is array (0 to 255) of std_logic_vector(31 downto 0);
 
  impure function read_ram_data return ram_type is
    file ram_file: text open read_mode is "ram_data.txt";
    variable txt_line: line;
    variable ram_data: ram_type;
  begin
    for i in 0 to 255 loop
      readline(ram_file, txt_line);
      hread(txt_line, ram_data(i));
    end loop;
    return ram_data;
  end function;
  
  procedure write_ram_data(ram_data : ram_type) is
    file ram_file: text open write_mode is "ram_data_out.txt";
    variable txt_line: line;
    begin
        for i in 0 to 255 loop
          write(txt_line, ram_data(i));
          writeline(ram_file, txt_line);
        end loop;
  end procedure;
  
  --signal ram: ram_type := read_ram_data;
  --signal ram_result: ram_type := write_ram_data;
  
begin

  uut: mem_ram
    port map (
      clock => clock,
      we => we,
      address => address,
      datain => datain,
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
    for i in 0 to 255 loop
    address <= std_logic_vector(to_unsigned(i, 8));
    datain <= std_logic_vector(to_unsigned(i,30)) & "00";
    wait for 10 ns;
    we <= '1';
    wait for 10 ns;
    we <= '0';
    wait for 10 ns;
    end loop;
    --write_ram_data(ram);
    --Escrever dados testes na RAM
    --we <= '1';
    --for i in 0 to 255 loop
     --address <= std_logic_vector(to_unsigned(i, 8));
     --datain <= std_logic_vector(to_unsigned(i,30)) & "00";
     --dataout <= ram(to_integer(unsigned(address)));
     --wait for 10 ns;
    --end loop;
    
    --we <= '0';
    -- Ler data test da RAM
    --for i in 0 to 255 loop
      --address <= std_logic_vector(to_unsigned(i, 8));
      --dataout <= ram(to_integer(unsigned(address)));
      --wait for 10 ns;
      --assert dataout = ram(i)
       -- report "Unexpected data at address " & integer'image(i)
        --severity error;
    --end loop;
    wait;
  end process;
end Behavioral;