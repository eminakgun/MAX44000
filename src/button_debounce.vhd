----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/12/2019 10:40:51 AM
-- Design Name: 
-- Module Name: button_debounce - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity button_debounce is

	Generic(
				clk_f : integer := 100_000_000
		    );
	Port ( 
			clk 	: in  STD_LOGIC ;
			dataIn  : in  STD_LOGIC ;
			dataOut : out STD_LOGIC 
		  );
		  
end button_debounce;

architecture Behavioral of button_debounce is

constant cntr10msLim : integer := clk_f / 100;
constant cntr50msLim : integer := cntr10msLim*5;
signal cntr10ms : integer range 0 to cntr50msLim := 0 ;

type states is (zero,zerotoone,one,onetozero);
signal state : states := zero ;

signal dataOut_reg : STD_LOGIC := '0';


begin

    dataOut <= dataOut_reg ;

	PROCESS(clk)
		begin
		
			if(clk'event and clk = '1') then
			
				case(STATE) is
				
					when ZERO =>
					    dataOut_reg <= '0';
						if(dataIn = '1') then
							STATE <= ZEROTOONE ;
						end if;
						
						
					when ZEROTOONE =>
						if(cntr10ms = cntr10msLim-1) then
							cntr10ms <= 0 ;
							if(dataIn = '1') then
								STATE <= ONE  ;
							else
								STATE <= ZERO ;
							end if;
						else
							cntr10ms <= cntr10ms + 1;
						end if;
					
					
					when ONE =>
					    dataOut_reg <= '1' ;
						if(dataIn = '0') then
							STATE <= ONETOZERO ;
						end if;
					
					
					when ONETOZERO =>
						if(cntr10ms = cntr10msLim-1) then
							cntr10ms <= 0 ;
							if(dataIn = '0') then
								STATE <= ZERO  ;
							else
								STATE <= ONE ;
							end if;
						else
							cntr10ms <= cntr10ms + 1;
						end if;
				
				end case;
			end if; -- clk'event end
		end PROCESS;


end Behavioral;
