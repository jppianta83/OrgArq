library IEEE;
use IEEE.std_logic_1164.all;
use work.p_MR2.all;

entity UART is
	port
	(
		ck,rst, ce, rw: in std_logic;
		address: in reg32;
		data: inout reg32;
		inta: in std_logic;
		intr: out std_logic
			-- falta definir a interface com a outra UART (modelo assincrono) --
	);
end UART;

architecture UART of UART is
begin
			-- Implementação da UART --
end UART;