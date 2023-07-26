-- Code your design here
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

--Somador
entity Add is
	port(
    	val1	: in std_logic_vector(31 downto 0);
        val2	: in std_logic_vector(31 downto 0);
        
        result	: out std_logic_vector(31 downto 0)
    );
end Add;

architecture behavior of Add is
begin
	process(val1, val2) begin
		result <= std_logic_vector(signed(val1) + signed(val2));
    end process;
end behavior;

--MUX
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity MUX is
    port(
        caso1	: in std_logic_vector(31 downto 0);
        caso2	: in std_logic_vector(31 downto 0);
        decisor	: in std_logic;
        
        saida: out std_logic_vector(31 downto 0)
        );
end MUX;

architecture behaviour of MUX is
begin
    process(decisor, caso1, caso2) begin
        case decisor is
            when '0' => saida <= caso1;
            when '1' => saida <= caso2;
            when others => saida <= x"00000000";
        end case;
   end process;
end behaviour;

--PC
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity PC is
	port(
	clk: in std_logic;
	
        recieved: in std_logic_vector(31 downto 0);
        
        address	: out std_logic_vector(31 downto 0)
    );
end PC;

architecture behavior of PC is
begin
	process(clk, recieved) begin
	if (rising_edge(clk)) then
    		address <= recieved;
	end if;
    end process;
end behavior;

--Memoria de Instruções
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
Read:
	-- read_address <= address;
    process(address) begin
		dataout <= mem(to_integer(unsigned(address(7 downto 2))));
    end process;
end RTL;

--Armazenadores IF/ID
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity regIFID is
	port(
    	clk	:	 in std_logic;
        PC		: in std_logic_vector(31 downto 0);
        ins		: in std_logic_vector(31 downto 0);
        
        rs1Prev	: out std_logic_vector(4 downto 0);
        PCPrev	: out std_logic_vector(31 downto 0);
        insPrev	: out std_logic_vector(31 downto 0)
	);
end regIFID;

architecture behavior of regIFID is
begin
	process(clk, PC, ins) begin
      if (rising_edge(clk)) then
          PCPrev <= PC;
          insPrev <= ins;
          if (ins(6 downto 0) = "0110111" or ins(6 downto 0) = "1101111") then
              rs1Prev <= "00000";  --Gambiarra pro LUI e pro jal
          else
              rs1Prev <= ins(19 downto 15);
          end if;
      end if;
    end process;
end architecture;

--Controle
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity control is
	port(
        instruction					: in std_logic_vector(31 downto 0);
        
        ALUSrc						: out std_logic;
        ALUOp						: out std_logic_vector(1 downto 0);
        Branch, MemRead, MemWrite	: out std_logic;
        RegWrite, Mem2Reg			: out std_logic;

	incondicional			:out std_logic
	);
end control;

architecture behavior of control is
begin
	process(instruction) begin
        	case  '0' & instruction(6 downto 0) is
              -- R TYPE (lógico aritméticas)
              when x"33" => ALUSrc <= '0';
                ALUOp <= "10";
                Branch <= '0';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '0';

              -- I TYPE (Load word)
              when x"03" => ALUSrc <= '1';
                ALUOp <= "00";
                Branch <= '0';
                MemRead <= '1';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '1';
                incondicional <= '0';

              -- I TYPE (Logico aritméticas com imediatos)
              when x"13" => ALUSrc <= '1';
                ALUOp <= "11";
                Branch <= '0';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '0';

              -- I TYPE (Jalr)
              when x"67" => ALUSrc <= '0';
                ALUOp <= "11";
                Branch <= '1';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '1';

              -- S TYPE (store w)
              when x"23" => ALUSrc <= '1';
                ALUOp <= "00";
                Branch <= '0';
                MemRead <= '0';
                Memwrite <= '1';
                RegWrite <= '0';
                Mem2Reg <= 'X';
                incondicional <= '0';

              -- SB TYPE (branch)
              when x"63" => ALUSrc <= '0';
                ALUOp <= "01";
                Branch <= '1';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '0';
                Mem2Reg <= 'X';
                incondicional <= '0';
                
				-- U TYPE (lui, auipc)
              when x"37" => ALUSrc <= '1';
                ALUOp <= "11";
                Branch <= '0';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '0';
              	--auipc
              when x"17" => ALUSrc <= '1';
                ALUOp <= "11";
                Branch <= '0';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '0';

              -- UJ TYPE (jal)
              when x"6F" => ALUSrc <= '0';
                ALUOp <= "11";
                Branch <= '1';
                MemRead <= '0';
                Memwrite <= '0';
                RegWrite <= '1';
                Mem2Reg <= '0';
                incondicional <= '1';

              when others => ALUSrc <= 'X';
                ALUOp <= "XX";
                Branch <= 'X';
                MemRead <= 'X';
                Memwrite <= 'X';
                RegWrite <= 'X';
                Mem2Reg <= 'X';
                incondicional <= 'X';
            end case;
    end process;
end behavior;

--Banco de Registradores
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity XREGS is
	generic (WSIZE : natural := 32);
    port (
    	clk, wren, rst	: in std_logic;
        rs1, rs2, rd	: in std_logic_vector(4 downto 0);
        data			: in std_logic_vector(WSIZE-1 downto 0);
        
        ro1, ro2		: out std_logic_vector(WSIZE-1 downto 0)
	);
end XREGS;

architecture behavior of XREGS is
	type regArray is array(natural range <>) of std_logic_vector(31 downto 0);
    signal breg: regArray(31 downto 0);
begin
	process(clk, rst, rs1, rs2, rd, data, breg) begin
    	breg(0) <= X"00000000";
    	if (rst = '1') then
        	breg(to_integer(unsigned(rd))) <= X"00000000";
        	ro1 <= X"00000000";
            ro2 <= X"00000000";
        else
            ro1 <= breg(to_integer(unsigned(rs1)));
            ro2 <= breg(to_integer(unsigned(rs2)));
        	if (rising_edge(clk)) then
              if (wren = '1' and rd /= "00000") then
                  breg(to_integer(unsigned(rd))) <= data;
              end if;
            end if;
        end if;
    end process;
end behavior;

--Gerador de Imediatos Gabi
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity genImm32 is
	port	(
				instr : in std_logic_vector(31 downto 0);
				imm32 : out signed(31 downto 0)
        	);
end entity;

architecture df of genImm32 is
begin
	process (instr)
    begin
    	case instr(6 downto 0) is
          when "0110011" => imm32 <= X"00000000";
          when "0000011" => imm32 <= resize(signed(instr(31 downto 20)), 32);
          when "0010011" => imm32 <= resize(signed(instr(31 downto 20)), 32);
          when "1100111" => imm32 <= resize(signed(instr(31 downto 20)), 32);
          when "0100011" => imm32 <= resize(signed(instr(31 downto 25) & instr(11 downto 7)), 32);
          when "1100011" => imm32 <= resize(signed(instr(31) & instr(7) & instr(30 downto 25) & instr(11 downto 8) & '0'), 32);
          when "1101111" => imm32 <= resize(signed(instr(31) & instr(19 downto 12) & instr(20) & instr(30 downto 21)), 32);
          when "0110111" => imm32 <= signed(instr(31 downto 12) & "000000000000");
          when "0010111" => imm32 <= signed(instr(31 downto 12) & "000000000000");
          when others => imm32 <= "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
      end case;
    end process;
end df;

--Armazenadores ID/EX
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity regIDEX is
	port(
    	clk										: in std_logic;
        PC										: in std_logic_vector(31 downto 0);
        ro1, ro2 								: in std_logic_vector(31 downto 0);
        imm32									: in std_logic_vector(31 downto 0);
        instruction								: in std_logic_vector(31 downto 0);
        ALUSrc									: in std_logic;
        ALUOp									: in std_logic_vector(1 downto 0);
        Branch, MemRead, MemWrite				: in std_logic;
        RegWrite, Mem2Reg						: in std_logic;
	incondicional								: in std_logic;
        
        PCPrev									: out std_logic_vector(31 downto 0);
        ro1Prev, ro2Prev 						: out std_logic_vector(31 downto 0);
        imm32Prev								: out std_logic_vector(31 downto 0);
        funct3Prev								: out std_logic_vector(2 downto 0);
        funct7Prev								: out std_logic_vector(6 downto 0);
        destRegPrev								: out std_logic_vector(4 downto 0);
        --EX
        ALUSrcPrev								: out std_logic;
        ALUOpPrev								: out std_logic_vector(1 downto 0);
        --M
        BranchPrev, MemReadPrev, MemWritePrev	: out std_logic;
        --WB
        RegWritePrev, Mem2RegPrev				: out std_logic;

	
	incondicionalPrev								: out std_logic
	);
end regIDEX;

architecture behavior of regIDEX is
begin
	process(clk, PC, ro1, ro2, imm32, instruction, ALUSrc, ALUOp, Branch, MemRead, MemWrite, RegWrite, Mem2Reg) begin
      if (rising_edge(clk)) then
          if (instruction(6 downto 0) = "1101111" or instruction(6 downto 0) = "1100111") then
              ro1Prev <= X"00000000";
              ro2Prev <= std_logic_vector(unsigned(PC) + X"4"); --Gambiarra pro jal e jalr
          elsif (instruction(6 downto 0) = "0010111") then
              ro2Prev <= ro2;
              ro1Prev <= PC; --Gambiarra pro AUIPC
          else 
              ro2Prev <= ro2;
              ro1Prev <= ro1;
          end if;

          --if (instruction(6 downto 0) = "1100111") then
          --    PCPrev <= ro1; --Gambiarra pro jalr
	  --else
          PCPrev <= PC;
          --end if;

	  --if (instruction(6 downto 0) = "0010111" or instruction(6 downto 0) = "1101111" or instruction(6 downto 0) = "1100111" or instruction(6 downto 0) = "1100011")  then
	  --    imm32Prev <= std_logic_vector(unsigned(imm32) - X"00400000");
	  --else
          imm32Prev <= imm32;
	  --end if;

          if (instruction(6 downto 0) = "0110111" or instruction(6 downto 0) = "0010111" or instruction(6 downto 0) = "1101111") then
              funct3Prev <= "000"; -- Gambiarra pro lui e AUIPC e jal
          else
              funct3Prev <= instruction(14 downto 12);
          end if;

          funct7Prev <= instruction(31 downto 25);
          destRegPrev <= instruction(11 downto 7);
          ALUSrcPrev <= ALUSrc;
          ALUOpPrev <= ALUOp;
          BranchPrev <= Branch;
          MemReadPrev <= MemRead;
          MemWritePrev <= MemWrite;
          RegWritePrev <= RegWrite;
          Mem2RegPrev <= Mem2Reg;
	  incondicionalPrev <= incondicional;
      end if;
    end process;
end architecture;

-- Controle da ULA
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity ALU_control is
	port(
    	ALUOp	: in std_logic_vector(1 downto 0);
        funct7	: in std_logic_vector(6 downto 0);
        funct3	: in std_logic_vector(2 downto 0);
        instr	: out std_logic_vector(3 downto 0)
    );
end ALU_control;

architecture behavior of ALU_control is
begin
	process(ALUOp, funct3, funct7) begin
      case (ALUOp) is
          when "00" => instr <= X"0"; --load e store
          when "01" => case (funct3) is
                  when "000" => instr <= X"C"; --beq
                  when "001" => instr <= X"D"; --bne
                  when "100" => instr <= X"8"; --blt
                  when "101" => instr <= X"A"; --bge
                  when "110" => instr <= X"9"; --bltu
                  when "111" => instr <= X"B"; --bgeu
                  when others => instr <= "XXXX";
              end case;
          when "10" => case (funct3) is
                  when "111" => instr <= X"2"; --and
                  when "110" => instr <= X"3"; --or
                  when "100" => instr <= X"4"; --xor
                  when "001" => instr <= X"5"; --sll
                  when "101" => case(funct7) is
                          when "0000000" => instr <= X"6"; --srl
                          when "0100000" => instr <= X"7"; --sra
                          when others => instr <= "XXXX";
                      end case;
                  when "010" => instr <= X"8"; --slt
                  when "011" => instr <= X"9"; --sltu
                  when "000" => case(funct7) is
                          when "0000000" => instr <= X"0"; --add
                          when "0100000" => instr <= X"1"; --sub
                          when others => instr <= "XXXX";
                      end case;
                  when others => instr <= "XXXX";
              end case;
          when "11" => case (funct3) is
                  when "000" => instr <= X"0"; --addi e jalr e jal
                  when "111" => instr <= X"2"; --andi
                  when "110" => instr <= X"3"; --ori
                  when "100" => instr <= X"4"; --xori
                  when "001" => instr <= X"5"; --slli
                  when "101" => case(funct7) is
                          when "0000000" => instr <= X"6"; --srli
                          when "0100000" => instr <= X"7"; --srai
                          when others => instr <= "XXXX";
                      end case;
                  when "010" => instr <= X"8"; --slti
                  when "011" => instr <= X"9"; --sltiu
                  when others => instr <= "XXXX";
              end case;
          when others => instr <= "XXXX";
      end case;
    end process;
end behavior;

-- ULA Gabi
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity ula_RV is
    generic(WSIZE: natural := 32);
    port(
        opcode: in std_logic_vector(3 downto 0);
        A,B: in std_logic_vector(WSIZE-1 downto 0);
        Z: out std_logic_vector(WSIZE-1 downto 0)
    );
end ula_RV;

architecture behavior of ula_RV is
    signal a32: std_logic_vector(31 downto 0);
    begin
        Z <= a32;
        proc_ula: process(A, B, opcode, a32) begin

            case (opcode) is
                --add
                when x"0" => a32 <= std_logic_vector(signed(A) + signed(B));
                --sub
                when x"1" => a32 <= std_logic_vector(signed(A) - signed(B));
                --and
                when x"2" => a32 <= std_logic_vector(A and B);
                --or
                when x"3" => a32 <= std_logic_vector(A or B);
                --xor
                when x"4" => a32 <= std_logic_vector(A xor B);
                --sll
                when x"5" => a32 <= std_logic_vector(shift_left(unsigned(A), to_integer(unsigned(B))));
                --srl
                when x"6" => a32 <= std_logic_vector(signed(A) srl to_integer(unsigned(B)));
                --sra
                when x"7" => a32 <= std_logic_vector(signed(A) sra to_integer(signed(B)));
                --slt
                when x"8" => if(signed(A) < signed(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
				--sltu
                when x"9" => if (unsigned(A) < unsigned(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
                --sge
                when x"A" => if(signed(A) >= signed(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
                --sgeu
                when x"B" => if(unsigned(A) >= unsigned(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
                --seq
                when x"C" => if(signed(A) = signed(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
                --sne
                when x"D" => if(signed(A) /= signed(B)) then a32 <= (0 => '1', others => '0');
                                else a32 <= (others => '0');
                                end if;
                when others => a32 <= x"00000000";
            end case;
        end process;
end behavior;

--Armazenadores EX/MEM
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity regEXMEM is
	port(
    	clk										: in std_logic;
        PC										: in std_logic_vector(31 downto 0);
        PCAdd									: in std_logic_vector(31 downto 0);
        ALUresult								: in std_logic_vector(31 downto 0);
        destReg									: in std_logic_vector (4 downto 0);
        ro2 									: in std_logic_vector(31 downto 0);
        Zero									: in std_logic;
        Branch, MemRead, MemWrite				: in std_logic;
        RegWrite, Mem2Reg						: in std_logic;
	incondicional								: in std_logic;
        
        PCPrev									: out std_logic_vector(31 downto 0);
        PCAddPrev								: out std_logic_vector(31 downto 0);
        ALUresultPrev							: out std_logic_vector(31 downto 0);
        destRegPrev								: out std_logic_vector (4 downto 0);
        ro2Prev 								: out std_logic_vector(31 downto 0);
        ZeroPrev								: out std_logic;
        --M
        BranchPrev, MemReadPrev, MemWritePrev	: out std_logic;
        --WB
        RegWritePrev, Mem2RegPrev				: out std_logic;
	incondicionalPrev							: out std_logic
	);
end regEXMEM;

architecture behavior of regEXMEM is
begin
	process(clk, PC, PCAdd, ALUresult, destReg, ro2, Zero, Branch, MemRead, MemWrite, RegWrite, Mem2Reg) begin
      if (rising_edge(clk)) then
          PCPrev <= PC;
          PCAddPrev <= PCAdd;
          ro2Prev <= ro2;
          ZeroPrev <= Zero;
          destRegPrev <= destReg;
          BranchPrev <= Branch;
          MemReadPrev <= MemRead;
          MemWritePrev <= MemWrite;
          RegWritePrev <= RegWrite;
          Mem2RegPrev <= Mem2Reg;
          ALUresultPrev <= ALUresult;
	  incondicionalPrev <= incondicional;
      end if;
    end process;
end architecture;

--RAM
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity mem_ram is
 port (
 	clk: in std_logic;
    we: in std_logic;
    address: in std_logic_vector(7 downto 0);
    datain: in std_logic_vector(31 downto 0);
    dataout: out std_logic_vector(31 downto 0)
    );
end mem_ram;

architecture RTL of mem_ram is

    type ram_type is array (0 to (2**address'length)-1) of std_logic_vector(datain'range);
    impure function read_ram_data return ram_type is
    file ram_file: text open read_mode is "ram_data.txt"; --WIP
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
    file ram_file: text open write_mode is "ram_data_out.txt"; --WIP
    variable txt_line: line;
    begin
        for i in 0 to 255 loop
          write(txt_line, ram_data(i));
          writeline(ram_file, txt_line);
        end loop;
    end procedure;

    signal mem: ram_type := read_ram_data;
    signal read_address : std_logic_vector(address'range);
    signal write_address : std_logic_vector(address'range);
    -- signal mem_result: ram_type;

begin
  Write:
  	process(clk, address) begin
      	write_address <= "00" & address(7 downto 2);
      if (we = '1' and rising_edge(clk)) then
          mem(to_integer(unsigned(write_address))) <= datain;
      end if;
    end process;

  Read: process(address) begin
    read_address <= "00" & address(7 downto 2);
    dataout <= mem(to_integer(unsigned(read_address)));
  end process;

  write_ram_data(mem);
end RTL;

--Armazenadores MEM/WB
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all; 
library std;
use std.textio.all;

entity regMEMWB is
	port(
    	clk										: in std_logic;
        PC										: in std_logic_vector(31 downto 0);
        PCAdd									: in std_logic_vector(31 downto 0);
        ALUresult								: in std_logic_vector(31 downto 0);
        Data									: in std_logic_vector(31 downto 0);
        destReg									: in std_logic_vector (4 downto 0);
        --WB
        RegWrite, Mem2Reg						: in std_logic;
        
        PCPrev									: out std_logic_vector(31 downto 0);
        PCAddPrev								: out std_logic_vector(31 downto 0);
        ALUresultPrev							: out std_logic_vector(31 downto 0);
        DataPrev								: out std_logic_vector(31 downto 0);
        destRegPrev								: out std_logic_vector (4 downto 0);
        --WB
        RegWritePrev, Mem2RegPrev				: out std_logic
	);
end regMEMWB;

architecture behavior of regMEMWB is
begin
	process(clk, PC, PCAdd, ALUresult, Data, destReg, RegWrite, Mem2Reg) begin
      if (rising_edge(clk)) then
          PCPrev <= PC;
          PCAddPrev <= PCAdd;
          DataPrev <= Data;
          destRegPrev <= destReg;
          RegWritePrev <= RegWrite;
          Mem2RegPrev <= Mem2Reg;
          ALUresultPrev <= ALUresult;
      end if;
    end process;
end architecture;