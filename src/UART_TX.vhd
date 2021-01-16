----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/05/2019 09:49:12 AM
-- Design Name: 
-- Module Name: UART_TX - Behavioral
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
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity UART_TX is
    Generic (
              BaudRate : integer := 115200 ;
              clk_f    : integer := 100000000  -- 100Mhz
            );
      Port (
                clk          : IN  STD_LOGIC ;
                SEND         : IN  STD_LOGIC ;
                TX_DOUT      : OUT STD_LOGIC ; 
				TX_DATA_IN   : IN  STD_LOGIC_VECTOR (7 DOWNTO 0) ; 
                TX_DONE_TICK : OUT STD_LOGIC 
            );
end UART_TX;

architecture Behavioral of UART_TX is

constant bit_time      : integer := clk_f / BaudRate;  
constant bit_count_lim : integer := bit_time - 1;
constant SB_lim : integer := bit_count_lim*2 ;
-----
type FSM is  (idle,START,SEND_DATA,STOP);
signal STATE : FSM := idle;
-----
signal n : integer range 0 to 7 := 0 ; -- data index counter
signal width_count : integer range 0 to bit_count_lim := 0 ; -- bit width time counter
-----
signal TX_DATA_IN_reg       :  STD_LOGIC_VECTOR ( 7 DOWNTO 0) := (others => '0') ;
signal TX_DONE_TICK_reg     :  STD_LOGIC := '0' ;
signal TX_DOUT_reg      	:  STD_LOGIC := '1' ;

begin

		TX_DONE_TICK <= TX_DONE_TICK_reg ;
		TX_DOUT      <= TX_DOUT_reg      ;
		
		process (clk) -- 100Mhz
            begin
				if (clk='1' and clk'EVENT)  then
				
					case STATE is
					
						when idle =>
						    TX_DONE_TICK_reg <= '0' ;
							TX_DOUT_reg      <= '1' ;
							if(SEND = '1') then
								TX_DATA_IN_reg <= TX_DATA_IN ; 
								STATE 	       <= START ;
							end if;
						
						when START =>
						
							TX_DOUT_reg <= '0' ;
							
							if (width_count = bit_count_lim) then
								width_count <= 0 ;
								STATE <= SEND_DATA ;
							else
								width_count <= width_count + 1 ;
							end if;
							
						
						when SEND_DATA =>
						
							TX_DOUT_reg <= TX_DATA_IN_reg(n) ;
							if (width_count = bit_count_lim) then
								width_count <= 0 ;
								if (n = 7) then
									n 	  <= 0 ;
									STATE <= STOP ;
								else
									n <= n + 1 ;
								end if ;
							else
								width_count <= width_count + 1     ;
							end if;
							
						when STOP =>
							TX_DOUT_reg <= '1' ;
							
							if (width_count = bit_count_lim) then
								width_count <= 0 ;
								TX_DONE_TICK_reg <= '1';
								STATE <= IDLE;
							else
								width_count <= width_count + 1 ;
							end if;
							
							
					end case; -- end case
				
				end if; -- end rising edge
			
			end process;

end Behavioral;