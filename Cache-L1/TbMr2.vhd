-------------------------------------------------------------------------
--
-- 32 bits PROCESSOR TESTBENCH - LITTLE  ENDIAN
--
-- It must be observed that the processor is hold in reset
-- (rstCPU <= '1') at the start of simulation, being activated
-- (rstCPU <= '0') just after the end of the object file reading be the
-- testbench.
--
-- This testbench employs two memories implying a HARVARD organization
--
-------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

package aux_functions is

	subtype reg32 is std_logic_vector(31 downto 0);
	subtype reg16 is std_logic_vector(15 downto 0);
	subtype reg8 is std_logic_vector(7 downto 0);
	subtype reg4 is std_logic_vector(3 downto 0);

	-- definição do tipo 'memory', que será utilizado para as memórias de dados/instruções
	constant MEMORY_SIZE: integer:= 2048;
	type memory is array (0 to MEMORY_SIZE) of reg8;
	constant TAM_LINHA: integer:= 200;
	function CONV_VECTOR(letra: string(1 to TAM_LINHA); pos: integer) return std_logic_vector;

end aux_functions;

package body aux_functions is

	--
	-- converte um caracter de uma dada linha em um std_logic_vector
	--
	function CONV_VECTOR(letra:string(1 to TAM_LINHA);  pos: integer) return std_logic_vector is
		variable bin: reg4;
	begin
		case (letra(pos)) is
				when '0' => bin:= "0000";
				when '1' => bin:= "0001";
				when '2' => bin:= "0010";
				when '3' => bin:= "0011";
				when '4' => bin:= "0100";
				when '5' => bin:= "0101";
				when '6' => bin:= "0110";
				when '7' => bin:= "0111";
				when '8' => bin:= "1000";
				when '9' => bin:= "1001";
				when 'A' | 'a' => bin:= "1010";
				when 'B' | 'b' => bin:= "1011";
				when 'C' | 'c' => bin:= "1100";
				when 'D' | 'd' => bin:= "1101";
				when 'E' | 'e' => bin:= "1110";
				when 'F' | 'f' => bin:= "1111";
				when others =>  bin:= "0000";
		end case;
	return bin;
	end CONV_VECTOR;

end aux_functions;

--------------------------------------------------------------------------
-- Module implementing a behavioral model of an ASYNCHRONOUS INTERFACE RAM
--------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_UNSIGNED.all;
use work.aux_functions.all;

entity RAM_mem is
	generic
	(
		START_ADDRESS: reg32:= (others => '0')
	);
	port
	(
		ce_n, we_n, oe_n, bw: in std_logic;
		address: in reg32;
		data: inout reg32
	);
end RAM_mem;

architecture RAM_mem of RAM_mem is
	signal RAM: memory;
	signal tmp_address: reg32;
	alias low_address: reg16 is tmp_address(15 downto 0);	--  baixa para 16 bits devido ao CONV_INTEGER --
begin

	tmp_address <= address - START_ADDRESS;	--  offset do endereçamento  --

	-- writes in memory ASYNCHRONOUSLY  -- LITTLE ENDIAN -------------------
	process(ce_n, we_n, low_address)
	begin
		if ce_n='0' and we_n='0' then
			if CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3) <= MEMORY_SIZE then
				if bw='1' then
					RAM(CONV_INTEGER(low_address+3)) <= data(31 downto 24);
					RAM(CONV_INTEGER(low_address+2)) <= data(23 downto 16);
					RAM(CONV_INTEGER(low_address+1)) <= data(15 downto  8);
				end if;
				RAM(CONV_INTEGER(low_address )) <= data(7 downto  0);
			end if;
		end if;
	end process;

	-- read from memory
	process(ce_n, oe_n, low_address)
	begin
		if ce_n='0' and oe_n='0' and CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3) <= MEMORY_SIZE then
			data(31 downto 24) <= RAM(CONV_INTEGER(low_address+3));
			data(23 downto 16) <= RAM(CONV_INTEGER(low_address+2));
			data(15 downto  8) <= RAM(CONV_INTEGER(low_address+1));
			data(7 downto  0) <= RAM(CONV_INTEGER(low_address ));
		else
			data(31 downto 24) <= (others => 'Z');
			data(23 downto 16) <= (others => 'Z');
			data(15 downto  8) <= (others => 'Z');
			data(7 downto  0) <= (others => 'Z');
		end if;
	end process;

end RAM_mem;



library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_UNSIGNED.all;
use work.aux_functions.all;

entity cache_L1 is
	port 
	(
		clk, ackin, sendin: in std_logic;
		ackout, sendout: out std_logic;
		dadoin: in std_logic_vector(127 downto 0); 
		--dadoin: in std_logic_vector(31 downto 0); 
		addressin: in std_logic_vector(31 downto 0):=(others => '0');
		addressout: out std_logic_vector(31 downto 0):=(others => '0');
		dataout: out reg32
	);
end entity;
architecture cache_L1 of cache_L1 is
	--signal linha0: std_logic_vector(158 downto 0):=(others => '1');
	--signal linha1: std_logic_vector(159 downto 0):=(others => '1');
	--signal linha2: std_logic_vector(159 downto 0):=(others => '1');
	--signal linha3: std_logic_vector(159 downto 0):=(others => '1');
	signal linhas: std_logic_vector(627 downto 0):=(others => '1');
	signal enderecoP: std_logic_vector(31 downto 0):=(others => '1');
	signal update: std_logic := '0';
	signal envio: std_logic := '0';
	signal linha: std_logic_vector(1 downto 0);
	signal bloco: std_logic_vector(1 downto 0);
begin
process(sendin)
begin
	if rising_edge(sendin) then
		if addressin(29 downto 28)="00" then
			linha <= "00";
			
			if linhas(28) = '1' then
				
			else
				 envio <= '1';
			end if;
		elsif addressin(29 downto 28)="01" then
			linha <= "01";
			if linhas(185) = '1' then
				sendout='1';
			else
				 envio <= '1';
			end if;
		elsif addressin(29 downto 28)="10" then
			linha <= "10";
			if linhas(342) = '1' then
				update <= '1';
			else
				 envio <= '1';
			end if;
		elsif addressin(29 downto 28)="11" then	
			linha <= "11";
			bloco <= "00";
			if linhas(499) = '1' then
				update <= '1';
			else
				 envio <= '1';
			end if;
		end if;
	end if;
end process;

process(update)
begin

	sendout <= '1';
end process;
process(ackin)
begin	
	if rising_edge(ackin) then
		if linha = "00" then
			linhas(60 downto 29) <= dadoin(31 downto 0);
			sendout <= '0';
			linhas(92 downto 61) <= dadoin(63 downto 32);
			sendout <= '0';
			linhas(124 downto 93) <= dadoin(95 downto 64);
			sendout <= '0';
			linhas(156 downto 125) <= dadoin(127 downto 96);
			linhas(28) <= '0';
			linhas(27 downto 0) <= addressin(27 downto 0);
			--linhas(1 downto 0) <= "00";
			sendout <= '0';
		elsif linha = "01" then
			linhas(217 downto 186) <= dadoin(31 downto 0);
			sendout <= '0';
			linhas(249 downto 218) <= dadoin(63 downto 32);
			sendout <= '0';
			linhas(281 downto 250) <= dadoin(95 downto 64);
			sendout <= '0';
			linhas(313 downto 282) <= dadoin(127 downto 96);
			linhas(185) <= '0';
			linhas(184 downto 157) <= addressin(27 downto 0);
			--linha1(1 downto 0) <= "00";
			sendout <= '0';
		elsif linha = "10" then
			linhas(374 downto 343) <= dadoin(31 downto 0);
			sendout <= '0';
			linhas(406 downto 375) <= dadoin(63 downto 32);
			sendout <= '0';
			linhas(438 downto 407) <= dadoin(95 downto 64);
			sendout <= '0';
			linhas(470 downto 439) <= dadoin(127 downto 96);
			linhas(342) <= '0';
			linhas(341 downto 314) <= addressin(27 downto 0);
			--linha2(341 downto 314) <= "00";
			sendout <= '0';
		else
			linhas(531 downto 500) <= dadoin(31 downto 0);
			linhas(563 downto 532) <= dadoin(63 downto 32);
			linhas(595 downto 564) <= dadoin(95 downto 64);
			linhas(627 downto 596) <= dadoin(127 downto 96);
			linhas(499) <= '0';
			linhas(498 downto 471) <= addressin(27 downto 0);
			--linha3(1 downto 0) <= "00";
			sendout <= '0';
		end if;
	end if;
	if falling_edge(ackin) then
		addressout <= "00" & linha & addressin(27 downto 0);
	end if;		
end process;
end architecture;
--process(envio)
--begin
	
--end process;



library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_UNSIGNED.all;
use work.aux_functions.all;

entity delay_RAM_mem is
	generic
	(
		START_ADDRESS: reg32:= (others => '0')
	);
	port
	(
		ack: out std_logic;
		gambiarrafeia, send ,clk, ce_n, we_n, oe_n, bw: in std_logic;
		address: in reg32;
		data: inout reg32
	);
end delay_RAM_mem;

architecture delay_RAM_mem of delay_RAM_mem is
	type type_state is (D0, D1, D2, D3, D4, D5, D6, D7, D8);
	signal Delay: type_state;
	signal RAM: memory;
	signal tmp_address: reg32;
	alias low_address: reg16 is tmp_address(15 downto 0);	--  baixa para 16 bits devido ao CONV_INTEGER --
begin



	tmp_address <= address - START_ADDRESS;	--  offset do endereçamento  --

	-- writes in memory ASYNCHRONOUSLY  -- LITTLE ENDIAN -------------------
	process(ce_n, we_n, low_address)
	begin
		if ce_n='0' and we_n='0' then
			if CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3) <= MEMORY_SIZE then
				if bw='1' then
					RAM(CONV_INTEGER(low_address+3)) <= data(31 downto 24);
					RAM(CONV_INTEGER(low_address+2)) <= data(23 downto 16);
					RAM(CONV_INTEGER(low_address+1)) <= data(15 downto  8);
				end if;
				RAM(CONV_INTEGER(low_address )) <= data(7 downto  0);
			end if;
		end if;
	end process;

	-- read from memory
	process(ce_n, oe_n, low_address, Delay, gambiarrafeia)
	begin
		if gambiarrafeia='0' then
			case Delay is
				when D8 =>
					--ack<='1';
					if ce_n='0' and oe_n='0' and CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3) <= MEMORY_SIZE then
						data(31 downto 24) <= RAM(CONV_INTEGER(low_address+3));
						data(23 downto 16) <= RAM(CONV_INTEGER(low_address+2));
						data(15 downto  8) <= RAM(CONV_INTEGER(low_address+1));
						data(7 downto  0) <= RAM(CONV_INTEGER(low_address ));
					else
						data(31 downto 24) <= (others => 'Z');
						data(23 downto 16) <= (others => 'Z');
						data(15 downto  8) <= (others => 'Z');
						data(7 downto  0) <= (others => 'Z');
					end if;
				when others =>
			end case;
		else 
			if ce_n='0' and oe_n='0' and CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3) <= MEMORY_SIZE then
				data(31 downto 24) <= RAM(CONV_INTEGER(low_address+3));
				data(23 downto 16) <= RAM(CONV_INTEGER(low_address+2));
				data(15 downto  8) <= RAM(CONV_INTEGER(low_address+1));
				data(7 downto  0) <= RAM(CONV_INTEGER(low_address ));
			else
				data(31 downto 24) <= (others => 'Z');
				data(23 downto 16) <= (others => 'Z');
				data(15 downto  8) <= (others => 'Z');
				data(7 downto  0) <= (others => 'Z');
			end if;
		end if;
	end process;



	process(clk)
	begin
		if falling_edge(clk) then
			case Delay is
				when D0 => 
					ack <= '0';
					if send='1' then
						Delay <= D1;
					end if;
				when D1 => Delay <= D2;
				when D2 => Delay <= D3;
				when D3 => Delay <= D4;
				when D4 => Delay <= D5;
				when D5 => Delay <= D6;
				when D6 => Delay <= D7;
				when D7 => Delay <= D8;
				when D8 => 
					ack <= '1';
					Delay <= D0;
			end case;
		end if;
	end process;
	


end delay_RAM_mem;


-------------------------------------------------------------------------
--  CPU PROCESSOR SIMULATION TESTBENCH
-------------------------------------------------------------------------
library ieee;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use STD.TEXTIO.all;
use work.aux_functions.all;

entity CPU_tb is
end CPU_tb;

architecture CPU_tb of CPU_tb is

	signal Dadress, Ddata, Iadress, Idata,
			i_cpu_address, d_cpu_address, data_cpu, tb_add, tb_data: reg32:= (others => '0');
	signal Dce_n, Dwe_n, Doe_n, Ice_n, Iwe_n, Ioe_n, ck, rst, rstCPU,
			go_i, go_d, rw, bw, gambiarrafeia: std_logic;
	signal ce: std_logic_vector(16 downto 0);
	signal intr, inta: std_logic;

	signal stx: std_logic;
	signal tx: std_logic_vector(3 downto 1);
	signal tx_ack: std_logic_vector(3 downto 1);
	signal external_data16: reg16;
	signal external_data: reg32;

	-- NOIS QI FIZEMU
	signal send_CPU, ack_CPU, ack_mem, send_cache, ack_cache, data_cache: std_logic;



	file ARQ: TEXT open READ_MODE is "start.txt";

begin

	rst <= '1', '0' after 5 ns;		-- generates the reset signal

	process						-- generates the clock signal
	begin
		ck <= '1', '0' after 5 ns;
		wait for 10 ns;
	end process;

	Data_mem: entity work.RAM_mem generic map(START_ADDRESS => x"10010000") port map(ce_n => Dce_n, we_n => Dwe_n, oe_n => Doe_n, bw => bw, address => Dadress, data => Ddata);

	Chace: entity work.cache_L1 port map(clk=>ck, ackin=>ack_mem, sendin=>send_CPU, ackout=>ack_CPU, datain=>Idata);

	Instr_mem: entity work.Delay_RAM_mem generic map(START_ADDRESS => x"00400000") port map(gambiarrafeia=>gambiarrafeia, ack=> ack_cache, send=>send_cache, clk=>ck, ce_n => Ice_n, we_n => Iwe_n, oe_n => Ioe_n, bw => '1', address => Iadress, data => data_cache);

	-- data memory signals --------------------------------------------------------
	Dce_n    <= '0' when ce(16)='1' or go_d='1' else '1';
	Doe_n    <= '0' when ce(16)='1' and rw='1' else '1';
	Dwe_n    <= '0' when (ce(16)='1' and rw='0') or go_d='1' else '1';
	Dadress  <= tb_add  when rstCPU='1' else d_cpu_address;
	Ddata	 <= tb_data when rstCPU='1' else data_cpu when (ce(16)='1' and rw='0') else (others => 'Z');
	data_cpu <= Ddata when ce(16)='1' and rw='1' else (others => 'Z');

	-- instructions memory signals --------------------------------------------------------
	Ice_n   <= '0';
	Ioe_n   <= '1' when rstCPU='1' else '0';			-- impede leitura enquanto está escrevendo
	Iwe_n   <= '0' when go_i='1'	else '1';			-- escrita durante a leitura do arquivo
	Iadress <= tb_add  when rstCPU='1' else i_cpu_address;
	Idata   <= tb_data when rstCPU='1' else (others => 'Z');

	----------------------------------------------------------------------------
	-- this process loads the instruction memory and the data memory during reset
	--
	--
	--	O PROCESSO ABAIXO É UMA PARSER PARA LER CÓDIGO GERADO PELO MARS NO
	--	SEGUINTE FORMATO:
	--
	--		.CODE
	--		0x00400000  0x08100022  j 0x00400088          7            j MyMain
	--		0x00400004  0x3c010000  lui $1,0x0000         12           subu $sp, $sp, 4
	--		0x00400008  0x34210004  ori $1,$1,0x0004           
	--		0x0040000c  0x03a1e823  subu $29,$29,$1            
	--		...
	--		0x0040009c  0x01285021  addu $10,$9,$8        65           addu $t2, $t1, $t0
	--		0x004000a0  0x08100025  j 0x00400094          66           j SaltoMyMain
	--
	--		.DATA
	--		0x10010000	0x0000faaa  0x00000083  0x00000000  0x00000000
	--
	----------------------------------------------------------------------------
	process
		variable ARQ_LINE: LINE;
		variable line_arq: string(1 to 200);
		variable code	: boolean;
		variable i, address_flag: integer;
	begin	
		gambiarrafeia <= '1';
		go_i <= '0';
		go_d <= '0';
		rstCPU <= '1';			-- hold the processor during file reading
		code:=true;				-- default value of code is 1 (CODE)

		wait until rst = '1';

		while NOT (endfile(ARQ)) loop	-- INÍCIO DA LEITURA DO ARQUIVO CONTENDO INSTRUÇÃO E DADOS -----
			readline(ARQ, ARQ_LINE);
			read(ARQ_LINE, line_arq(1 to  ARQ_LINE'length));

			if line_arq(1 to 5)=".CODE" then code := true;							-- code
			elsif line_arq(1 to 5)=".DATA" then code := false;						-- data
			else
				i := 1;								-- LEITORA DE LINHA - analizar o loop abaixo para compreender
				address_flag := 0;					-- para INSTRUÇÃO é um para (end,inst)
													-- para DADO aceita (end, dado 0, dado 1, dado 2 ....)
				loop
					if line_arq(i) = '0' and line_arq(i+1) = 'x' then	-- encontrou indicação de número hexa: '0x'
						i:= i + 2;
						if address_flag=0 then
							for w in 0 to 7 loop
								tb_add((31-w*4) downto (32-(w+1)*4))  <= CONV_VECTOR(line_arq,i+w);
							end loop;
							i:= i + 8;
							address_flag:= 1;
						else
							for w in 0 to 7 loop
								tb_data((31-w*4) downto (32-(w+1)*4))  <= CONV_VECTOR(line_arq,i+w);
							end loop;
							i:= i + 8;
							wait for 0.1 ns;
							if code=true then go_i <= '1';	-- the go_i signal enables instruction memory writing
											else go_d <= '1';	-- the go_d signal enables data memory writing
							end if;
							wait for 0.1 ns;
							tb_add <= tb_add + 4;		-- *great!* consigo ler mais de uma word por linha!
							go_i <= '0';
							go_d <= '0';
							address_flag:= 2;	-- sinaliza que já leu o conteúdo do endereço;
						end if;
					end if;
					i:= i + 1;
					-- sai da linha quando chegou no seu final OU já leu par(endereço, instrução) no caso de código
					exit when i=TAM_LINHA or (code=true and address_flag=2);
				end loop;
			end if;
		end loop;								-- FINAL DA LEITURA DO ARQUIVO CONTENDO INSTRUÇÃO E DADOS -----
		rstCPU <= '0';	-- release the processor to execute
		wait for 2 ns;	-- To activate the RST CPU signal
		wait until rst = '1';  -- to Hold again!
		gambiarrafeia <= '1';
	end process;

	-- Port map dos subsitemas ------------------------------------------------------------
	---------------------------------------------------------------------------------------
	CPU: Entity work.MR2 port map
		(send => send_CPU, ack => ack_CPU, clock => ck, reset => rstCPU, i_address => i_cpu_address, instruction => Idata,
		ce => ce, rw => rw, bw => bw, d_address => d_cpu_address, data => data_cpu,
		intr => intr, inta => inta);


	---------------------------------------------------------------------------------------
	---	Deve ser inserido um codigo que complementa a funcionalidade da UART
	---------------------------------------------------------------------------------------

end CPU_tb;
