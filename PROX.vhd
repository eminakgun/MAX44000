----------------------------------------------------------------------------------
-- Company:  
-- Engineer:
-- 
-- Create Date: 03/05/2019 10:10:58 AM
-- Design Name: 
-- Module Name: PROX - Behavioral
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

entity PROX is
	GENERIC (
				clk_f 	: integer := 100_000_000 ;
				SCKFREQ	: integer := 400_000	 ;
				FIFO_depth : integer := 1024 ;
				Data_width : integer :=    8 -- fifo data width
			);
	PORT ( 
			CLK      : IN    STD_LOGIC ;
			BUT1     : IN    STD_LOGIC ;
			BUT2     : IN    STD_LOGIC :='0';
			RST      : IN    STD_LOGIC ; -- ACTIVE LOW RESET
			--trig    : IN  STD_LOGIC ;    
			TX_DOUT  : OUT   STD_LOGIC ;
			READ_LED : OUT   STD_LOGIC ;
			RST_LED : OUT   STD_LOGIC ;
			ACKERR_LED : OUT STD_LOGIC ; 
			SDA	  	 : INOUT STD_LOGIC ; 
			SCL	  	 : INOUT STD_LOGIC 
		 );
end PROX;

architecture Behavioral of PROX is

COMPONENT fifo
		Generic(
					FIFO_depth : integer := 1024 ;
					Data_width : integer :=    8
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
END COMPONENT;


COMPONENT UART_TX 
	GENERIC (
              BaudRate : integer := 115200 ;
              clk_f    : integer := 100_000_000  -- 100Mhz
            );
      PORT (
                clk          : IN  STD_LOGIC ;
                SEND         : IN  STD_LOGIC ;
                TX_DOUT      : OUT STD_LOGIC ; 
				TX_DATA_IN   : IN  STD_LOGIC_VECTOR (7 DOWNTO 0) ; 
                TX_DONE_TICK : OUT STD_LOGIC 
            );
END COMPONENT;


COMPONENT i2c_master
	GENERIC(
				input_clk : INTEGER := 25_000_000; --input clock speed from user logic in Hz
				bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
      PORT(
			clk       : IN     STD_LOGIC;                    --system clock
			reset_n   : IN     STD_LOGIC;                    --active low reset
			ena       : IN     STD_LOGIC;                    --latch in command
			addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
			rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
			data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
			busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
			data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
			ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
			sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
			scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END COMPONENT;


COMPONENT button_debounce 

	GENERIC (
				clk_f : integer := 100_000_000
		    );
	PORT    ( 
				clk 	: IN  STD_LOGIC ;
				dataIn  : IN  STD_LOGIC ;
				dataOut : OUT STD_LOGIC 
		    );
		  
end COMPONENT;

signal RST_n          : STD_LOGIC := '1'     		;
signal BUT1_DEB 	  : STD_LOGIC := '0'     		;
signal BUT1_DEB_prev  : STD_LOGIC := '0'     		;
signal BUT2_DEB 	  : STD_LOGIC := '0'     		;
signal BUT2_DEB_prev  : STD_LOGIC := '0'     		;
signal RST_DEB  	  : STD_LOGIC := '0'     		;
signal RST_DEB_prev   : STD_LOGIC := '0'     		;
signal rst_cnt  	  : integer range 0 to 5 := 0 ;

constant sckTimeLim	: integer := clk_f/SCKFREQ;
signal   sckTime    : integer  range 0 to sckTimeLim := 0;
constant Lim1s  	: integer := clk_f ;
signal   cntr1s 	: integer range 0 to Lim1s := 0;

-- UART_TX Signals
signal TX_DATA_IN    : STD_LOGIC_VECTOR (7 DOWNTO 0) := (others => '0') ;
--signal TX_DOUT   : STD_LOGIC := '1' ;
signal TX_DONE_TICK  : STD_LOGIC        ;
signal SEND 	     : STD_LOGIC := '0' ;

--signal TX_DONE_TICK_prev  : STD_LOGIC   	 ;
--signal state_en           : STD_LOGIC := '1' ;
--signal uart_cnt : integer range 0 to 1 := 0  ;
-----------------------------------------------------------

-- i2c_master Signals
signal ena			  : std_logic := '0';
constant addr   	  : std_logic_vector (6 downto 0) := "1001010" ; -- Slave Address
signal rw       	  : std_logic := '0';
signal data_wr  	  : std_logic_vector (7 downto 0) := (others => '0');
signal busy     	  : std_logic := '0';
signal busyPrev       : std_logic := '0';
signal busyCntr		  : integer range 0 to 255 := 0;
signal data_rd  	  : std_logic_vector (7 downto 0) := (others => '0');
signal ack_error	  : std_logic := '0';
signal ack_error_prev : std_logic := '1';
---------------------------------------------------------------

--
signal INT_STATUS_REG : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal read_REG : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal wr_REG   : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal read_done	  : STD_LOGIC := '0' ;
signal read_done_prev : STD_LOGIC := '0' ;
signal write_done	  : STD_LOGIC := '0' ;
signal READ_LED_reg   : STD_LOGIC := '0' ;

-- FIFO Signals
signal FIFO_W : STD_LOGIC := '0' ;
signal FIFO_R : STD_LOGIC := '0' ;
signal DATA_W : STD_LOGIC_VECTOR(Data_width-1 DOWNTO 0) := (others => '0') ; 
signal DATA_R : STD_LOGIC_VECTOR(Data_width-1 DOWNTO 0) := (others => '0') ;
signal read_valid     : STD_LOGIC := '0' ; 
signal write_ack      : STD_LOGIC := '0' ; 
signal f_empty        : STD_LOGIC := '0' ; 
--signal uartFIFO_en : STD_LOGIC := '0' ;
-- END FIFO SIGNALS


----------------FSMs----------------
-- MAIN FSM --
type states is (RESET,IDLE,Read_Intreg,Read_Intreg_Nxt,Set_TransmitConfig,Set_MainConfig,Read_proxReg,TimeOut);
signal STATE : states := IDLE;
-- FIFO WR FSM --
type write_states is (IDLE,WRITE,WAIT_W);
signal STATE_W : write_states := IDLE ;
-- FIFO RD FSM --
type read_states is (IDLE,READ,WAIT_R);
signal STATE_R : read_states := IDLE ;

begin

FIFO_i : fifo
	GENERIC MAP (
					FIFO_depth => 512
				)
    PORT MAP (
				clk           =>  CLK,
				FIFO_W        => FIFO_W,
				FIFO_R        => FIFO_R,
				DATA_W   	  => DATA_W,
				DATA_R        => DATA_R,
				read_valid    => read_valid,
				write_ack     => write_ack,
				f_empty       => f_empty
			 );


UART_TX_i : UART_TX
	GENERIC MAP (
					clk_f    => clk_f,
					BaudRate => 115_200     
				)
				
	PORT MAP	(
					clk => clk,
					SEND => SEND,
					TX_DATA_IN => TX_DATA_IN,
					TX_DONE_TICK => TX_DONE_TICK,
					TX_DOUT => TX_DOUT
				);
				

i2c_master_i : i2c_master

	GENERIC MAP (
					input_clk	=> clk_f,
					bus_clk  	=> SCKFREQ
				)
	PORT MAP 	(
					clk      	=> clk,
					reset_n  	=> RST_n,
					ena      	=> ena,
					addr     	=> addr,
					rw       	=> rw,
					data_wr     => data_wr,
					busy        => busy,
					data_rd     => data_rd,
					ack_error   => ack_error,
					sda         => SDA,
					scl         => SCL
				);
				
but2_startInit : button_debounce
	GENERIC MAP(
				clk_f => clk_f 
				)
	PORT MAP ( 
				clk 	=> clk,
				dataIn  => BUT1,
				dataOut => BUT1_DEB
			 );
			 
but3_readProx : button_debounce
GENERIC MAP(
			clk_f => clk_f 
			)
PORT MAP ( 
			clk 	=> clk,
			dataIn  => BUT2,
			dataOut => BUT2_DEB
		 );
			 
but1_RST : button_debounce
	GENERIC MAP(
				clk_f => clk_f 
				)
	PORT MAP ( 
				clk 	=> clk,
				dataIn  => RST,
				dataOut => RST_DEB
			 );
				


	process (clk)
		begin
		
			if (RISING_EDGE(CLK)) then
			
				READ_LED 		  <= READ_LED_reg ;
				BUT1_DEB_prev     <= BUT1_DEB     ;
				BUT2_DEB_prev     <= BUT2_DEB     ;
				RST_DEB_prev  	  <= RST_DEB      ;
				--TX_DONE_TICK_prev <= TX_DONE_TICK ;
				read_done_prev    <= read_done    ;
				ack_error_prev    <= ack_error    ;
				
				-- if(TX_DONE_TICK_prev = '1' AND TX_DONE_TICK = '0') then
					-- state_en <= '1' ;
				-- end if;
				
				-- if(read_done_prev = '0' AND read_done = '1') then
					-- READ_LED_reg <= NOT READ_LED_reg ;
				-- end if;
				
				if(read_done_prev = '1' AND read_done = '0') then
					READ_LED_reg <= READ_LED_reg XOR READ_LED_reg ;
				elsif (read_done_prev = '0'AND read_done = '1') then
					READ_LED_reg <= READ_LED_reg XOR READ_LED_reg ;
				end if;
				
				if(ack_error_prev = '0' and ack_error = '1') then
					ACKERR_LED <= '1' ;
				-- elsif(ack_error_prev = '1' and ack_error = '0') then
					-- ACKERR_LED <= '0' ;
				end if;
				
				if(RST_DEB = '1' AND RST_DEB_prev = '0') then --i2c bus reset not IC
					RST_n      <= '0'   ;
					ACKERR_LED <= '0'   ;
					STATE      <= RESET ;
				end if;
				
				if(BUT1_DEB_prev = '0' AND BUT1_DEB = '1') then -- start initial configurations
					ACKERR_LED <= '0' ;
					STATE    <= READ_INTREG ; 
				end if;
				
				if(BUT2_DEB_prev = '0' AND BUT2_DEB = '1') then -- read prox reg consecutively
					ACKERR_LED <= '0' ;
					STATE    <= Read_proxReg ;
				end if;
			
				if( STATE = RESET) then
					RST_LED <= '1' ; 
				else
					RST_LED <= '0' ;
				end if;
			
				-- MAIN FSM --
				case STATE is
				
					when RESET => -- BUS RESET 
							
						RST_n <= '0';
						if(rst_cnt = 5) then
							rst_cnt  <= 0 	 ;
							RST_n   <= '1'   ;
							STATE    <= IDLE ;
						elsif(rst_cnt = 2) then
							rst_cnt <= rst_cnt + 1 ;
							RST_n   <= '1'		   ;
						else
							rst_cnt <= rst_cnt + 1 ;
						end if;
						
				
					when IDLE =>
						
						busyPrev  <= busy	  ;
						ena 	  <= '0' 	  ;
						read_done <= '0'      ;
						
						
					when Read_intReg =>
						
						busyPrev <= busy ;	
						if (busyPrev = '0' and busy = '1') then
							busyCntr <= busyCntr + 1 ; 
						end if;	
							
						
						if(busyCntr = 0) then
							ena 	<= '1'   ;
							rw  	<= '0'   ;  -- write operation
							data_wr <= x"00" ; -- int reg adress
						elsif(busyCntr = 1) then
							rw      <= '1' ;
						elsif(busyCntr = 2) then
							ena <= '0' ;
							if (busy = '0') then
								read_REG	   <= data_rd   ;
								read_done      <= '1'	    ;
								busyCntr	   <= 0		    ;
								STATE <= Set_TransmitConfig ;
							end if;
						end if;
						
						
					 when Set_TransmitConfig =>
					
						busyPrev <= busy ;	
						if (busyPrev = '0' and busy = '1') then
							busyCntr <= busyCntr + 1 ; 
						end if;
						
						if(busyCntr = 0) then
							ena 	<= '1'   ;
							rw  	<= '0'   ;  -- write operation
							data_wr <= x"03" ;  -- reg adress
						elsif(busyCntr = 1) then
							data_wr <= x"06" ;  -- data to be written
						elsif(busyCntr = 2) then
							ena <= '0' 		 ;
							if (busy = '0') then
								wr_REG 		   <= x"60"   			;
								write_done     <= '1'	  			;
								busyCntr	   <= 0		  		    ;
								STATE		   <= Set_MainConfig	;
							end if;
						end if;
						
					when Set_MainConfig =>
					
						busyPrev <= busy ;	
						if (busyPrev = '0' and busy = '1') then
							busyCntr <= busyCntr + 1 ; 
						end if;
						
						if(busyCntr = 0) then
							ena 	<= '1'   ;
							rw  	<= '0'   ;  -- write operation
							data_wr <= x"01" ;  -- reg adress
						elsif(busyCntr = 1) then
							data_wr <= x"34" ;  -- data to be written
						elsif(busyCntr = 2) then
							ena <= '0' ;
							if (busy = '0') then
								wr_REG 		   <= x"61"  	   ;
								write_done     <= '1'	  	   ;
								busyCntr	   <= 0		  	   ;
								STATE		   <= IDLE         ;
							end if;
						end if;
						
					when Read_proxReg =>
						
						busyPrev <= busy ;	
						if (busyPrev = '0' and busy = '1') then
							busyCntr <= busyCntr + 1 ; 
						end if;	
							
						
						if(busyCntr = 0) then
							ena 	<= '1'   ;
							rw  	<= '0'   ;  -- write operation
							data_wr <= x"16" ;  -- int reg adress
						elsif(busyCntr = 1) then
							rw      <= '1' ;
						elsif(busyCntr = 2) then
							ena 	<= '0' ;
							if (busy = '0') then
								read_REG	   <= data_rd	   ;
								read_done      <= '1'	       ;
								busyCntr	   <= 0	      	   ;
								STATE 		   <= IDLE ;
							end if;
						end if;
						
					when TimeOut =>
						
						if(cntr1s = Lim1s - 1) then
							cntr1s <= 0            ;
							STATE  <= Read_proxReg ;
						else
							cntr1s <= cntr1s + 1   ;
						end if;
					
						-- end if ;
						
					when OTHERS =>
						NULL ;
						
						
				end case;
				-- MAIN FSM END --
				
				-- FIFO WR FSM --
				case(STATE_W) is
					
							when IDLE =>
							
								if(read_done = '1') then -- or trig = '1') then
									DATA_W 	   <= read_REG ;
									read_done  <= '0'      ;
									STATE_W    <= WRITE    ;		
								elsif(write_done = '1') then
									write_done <= '0'      ;
									DATA_W     <= wr_REG   ;
									STATE_W    <= WRITE    ;
								else 
									STATE_W 	<= IDLE  ;
								end if;
									
							when WRITE =>
							
								FIFO_W 		<= '1' 		;
								STATE_W 	<= WAIT_W   ;
								
							when WAIT_W =>
								
								FIFO_W      <= '0' ;
								if(write_ack = '1') then
									STATE_W <= IDLE ;
								else
									STATE_W <= WAIT_W ;
								end if;
								
				end case;
				-- FIFO WR FSM  END --
				
				
				
				-- FIFO RD FSM + UART_TX --
				case(STATE_R) is
						
							when IDLE =>
							
								SEND 	 	  <= '0'  ;
								if(write_ack = '1') then
									FIFO_R    <= '1'  ;
									STATE_R   <= READ ;
								else
									FIFO_R 	  <= '0'  ;
								end if;
								
							when READ =>
								
								FIFO_R 	  <= '0' ;
								if(read_valid ='1') then
									TX_DATA_IN <= DATA_R ;
									SEND 	   <= '1'    ;
									STATE_R    <= WAIT_R ;
								end if;
								
							when WAIT_R =>
								
								SEND 	 	  <= '0'  ;
								if(TX_DONE_TICK = '1'  ) then
									if(f_empty = '1') then
										STATE_R   <= IDLE ;
										FIFO_R 	  <= '0'  ;
									else 
										STATE_R   <= READ ;
										FIFO_R    <= '1'  ;
									end if;
								else
									FIFO_R 	  <= '0'    ;
								    STATE_R   <= WAIT_R ;
								end if;
								
						end case;
					-- FIFO FSM END --
				
			end if;
        end PROCESS;
				
end Behavioral;
