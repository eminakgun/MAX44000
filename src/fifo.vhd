----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/15/2019 10:53:54 AM
-- Design Name: 
-- Module Name: fifo - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity fifo is
	Generic(
				FIFO_depth   : integer := 1024 ; 
				Data_width   : integer :=    8
			);
    PORT(   
            clk       : IN  STD_LOGIC ;
            FIFO_W    : IN  STD_LOGIC ;
            FIFO_R    : IN  STD_LOGIC ;
			DATA_W        : IN  STD_LOGIC_VECTOR (Data_width-1 DOWNTO 0)    ; 
			DATA_R        : OUT STD_LOGIC_VECTOR (Data_width-1 DOWNTO 0)    ;
			read_valid    : OUT STD_LOGIC       							;
			write_ack     : OUT STD_LOGIC									;
			f_empty       : OUT STD_LOGIC
        );
end fifo;

architecture Behavioral of fifo is

COMPONENT fifo_generator_0  --1024 data width
	  PORT (
				clk    : IN STD_LOGIC;
				rst    : IN STD_LOGIC;
				din    : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
				wr_en  : IN STD_LOGIC;
				rd_en  : IN STD_LOGIC;
				dout   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
				full   : OUT STD_LOGIC;
				wr_ack : OUT STD_LOGIC;
				empty  : OUT STD_LOGIC;
				valid  : OUT STD_LOGIC
			);
END COMPONENT;

-- FIFO Signals
signal fifo_rst : STD_LOGIC := '0';
signal fifo_din : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"00";
signal fifo_wr_en : STD_LOGIC := '0';
signal fifo_rd_en : STD_LOGIC := '0';
signal fifo_dout :  STD_LOGIC_VECTOR(7 DOWNTO 0);
signal fifo_full :  STD_LOGIC := '0' ;
signal fifo_wr_ack : STD_LOGIC := '0';
signal fifo_empty :  STD_LOGIC := '1';
signal fifo_valid :  STD_LOGIC := '0'; 
-- FIFO control signals
signal fifo_write :  STD_LOGIC := '0'; 
signal fifo_read  :  STD_LOGIC := '0'; 
signal fifo_cntr  : integer range 0 to 10 ; 
-- END FIFO SIGNALS
---------------------------------------
-- DATA PACKET
signal   DATA_W_reg       : STD_LOGIC_VECTOR (Data_width-1 DOWNTO 0) := (others => '0') ;

----------------
type fifo_states is (RESET,IDLE);
signal FIFO_s : fifo_states := RESET ;

type write_states is (IDLE,WRITE,WAITACK);
signal STATE_W : write_states := IDLE ;

type read_states is (IDLE,READ,WAITVALID);
signal STATE_R : read_states := IDLE ;

begin

your_instance_name : fifo_generator_0
  PORT MAP (
				clk => clk,
				rst => fifo_rst,
				din => fifo_din,
				wr_en => fifo_wr_en,
				rd_en => fifo_rd_en,
				dout => fifo_dout,
				full => fifo_full,
				wr_ack => fifo_wr_ack,
				empty => fifo_empty,
				valid => fifo_valid
			);


write_ack     <= fifo_wr_ack ;
read_valid    <= fifo_valid  ;
f_empty       <= fifo_empty	 ;
DATA_R 		  <= fifo_dout ;

	FIFO_fsm : process(clk)
                begin
                    if(RISING_EDGE(clk)) then
                    
						case(FIFO_s) is
							
							when RESET => 
							
								fifo_cntr <= fifo_cntr + 1 ;
									
									if(fifo_cntr = 0) then
										fifo_rst <= '1' ;  
										
									elsif(fifo_cntr = 5) then
										fifo_rst  <= '0'  ;  
										
									elsif(fifo_cntr = 10) then
										fifo_cntr <= 0    ;
										FIFO_s    <= IDLE ;
									end if;
							
							when IDLE =>
							
								case(STATE_W) is
								
									when IDLE =>
									
									   fifo_wr_en  <= '0' ;
									   
									    if(FIFO_W= '1') then 
                                            DATA_W_reg       <= DATA_W ;
											STATE_W     	 <= WRITE  ;
										else 
											--DATA_W_reg       <= (others => '0') ;
											STATE_W     	 <= IDLE  ;
                                        end if;
										
								
									when WRITE =>
									
										fifo_din   <= DATA_W_reg ;
										
										if(fifo_full /= '1') then
											fifo_wr_en <= '1'     ;
											STATE_W    <= WAITACK ;
										else 
											fifo_wr_en <= '0'   ;
											STATE_W    <= WRITE ;
										end if;
										
									when WAITACK =>
									
										fifo_wr_en <= '0' ;
										if (fifo_wr_ack = '1') then 
											STATE_W <= IDLE    ;  
										else
											STATE_W <= WAITACK ;
										end if;
								
								end case; -- end WRITE CASE
								
								
								case(STATE_R) is
								
									when IDLE =>
									   
										fifo_rd_en <= '0' 	   ;
									    if(FIFO_R= '1') then
											STATE_R    <= READ ;
                                        else
											STATE_R    <= IDLE ;
										end if;
								
									when READ =>
									
										if(fifo_empty /= '1') then
											fifo_rd_en <= '1'	    ;
											STATE_R    <= WAITVALID ;
										else
											fifo_rd_en <= '0'  ;
											STATE_R    <= READ ;
										end if;
										
									when WAITVALID =>   
									
                                        fifo_rd_en <= '0'   	   ;  
                                        if(fifo_valid = '1') then
                                           STATE_R    <= IDLE      ;
                                        else
										   STATE_R    <= WAITVALID ;
                                        end if;
								
								end case; -- end READ CASE
							
							end case ; -- end FIFO_s
					
			end if; -- end rising edge
		end process;
				
				


end Behavioral;
