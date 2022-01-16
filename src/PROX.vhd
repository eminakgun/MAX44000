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
-- use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
-- library UNISIM;
-- use UNISIM.VComponents.all;

entity PROX is
    generic (
        CLK_F      : INTEGER := 100_000_000;
        SCKFREQ    : INTEGER := 400_000;
        FIFO_DEPTH : INTEGER := 1024;
        DATA_WIDTH : INTEGER := 8 -- fifo data width
    );
    port (
        CLK        : in    STD_LOGIC;
        BUT1       : in    STD_LOGIC;
        BUT2       : in    STD_LOGIC;
        RST        : in    STD_LOGIC; -- ACTIVE LOW RESET
        -- trig    : IN  STD_LOGIC ;
        TX_DOUT    : out   STD_LOGIC;
        READ_LED   : out   STD_LOGIC;
        RST_LED    : out   STD_LOGIC;
        ACKERR_LED : out   STD_LOGIC;
        SDA        : inout STD_LOGIC;
        SCL        : inout STD_LOGIC
    );
end entity PROX;

architecture BEHAVIORAL of PROX is

    component FIFO is
        generic (
            FIFO_DEPTH : INTEGER := 1024;
            DATA_WIDTH : INTEGER := 8
        );
        port (
            CLK        : in    STD_LOGIC;
            FIFO_W     : in    STD_LOGIC;
            FIFO_R     : in    STD_LOGIC;

            DATA_W     : in    STD_LOGIC_VECTOR(DATA_WIDTH - 1 downto 0);
            DATA_R     : out   STD_LOGIC_VECTOR(DATA_WIDTH - 1 downto 0);
            READ_VALID : out   STD_LOGIC;
            WRITE_ACK  : out   STD_LOGIC;
            F_EMPTY    : out   STD_LOGIC
        );
    end component;

    component UART_TX is
        generic (
            BAUDRATE : INTEGER := 115200;
            CLK_F    : INTEGER := 100_000_000
        );
        port (
            CLK          : in    STD_LOGIC;
            SEND         : in    STD_LOGIC;
            TX_DOUT      : out   STD_LOGIC;
            TX_DATA_IN   : in    STD_LOGIC_VECTOR(7 downto 0);
            TX_DONE_TICK : out   STD_LOGIC
        );
    end component;

    component I2C_MASTER is
        generic (
            INPUT_CLK : INTEGER := 25_000_000;
            BUS_CLK   : INTEGER := 400_000
        );
        port (
            CLK       : in    STD_LOGIC;
            RESET_N   : in    STD_LOGIC;
            ENA       : in    STD_LOGIC;
            ADDR      : in    STD_LOGIC_VECTOR(6 downto 0);
            RW        : in    STD_LOGIC;
            DATA_WR   : in    STD_LOGIC_VECTOR(7 downto 0);
            BUSY      : out   STD_LOGIC;
            DATA_RD   : out   STD_LOGIC_VECTOR(7 downto 0);
            ACK_ERROR : buffer STD_LOGIC;
            SDA       : inout STD_LOGIC;
            SCL       : inout STD_LOGIC
        );
    end component;

    component BUTTON_DEBOUNCE is
        generic (
            CLK_F : INTEGER := 100_000_000
        );
        port (
            CLK     : in    STD_LOGIC;
            DATAIN  : in    STD_LOGIC;
            DATAOUT : out   STD_LOGIC
        );
    end component;

    signal rst_n         : std_logic := '1';
    signal but1_deb      : std_logic := '0';
    signal but1_deb_prev : std_logic := '0';
    signal but2_deb      : std_logic := '0';
    signal but2_deb_prev : std_logic := '0';
    signal rst_deb       : std_logic := '0';
    signal rst_deb_prev  : std_logic := '0';
    signal rst_cnt       : integer range 0 to 5 := 0;

    constant SCKTIMELIM : integer := CLK_F / SCKFREQ;
    signal   scktime    : integer  range 0 to SCKTIMELIM := 0;
    constant LIM1S      : integer := CLK_F;
    signal   cntr1s     : integer range 0 to LIM1S := 0;

    -- UART_TX Signals
    signal tx_data_in : std_logic_vector(7 downto 0) := (others => '0');
    -- signal TX_DOUT   : STD_LOGIC := '1' ;
    signal tx_done_tick : std_logic;
    signal send         : std_logic := '0';

    -- signal TX_DONE_TICK_prev  : STD_LOGIC      ;
    -- signal state_en           : STD_LOGIC := '1' ;
    -- signal uart_cnt : integer range 0 to 1 := 0  ;
    -----------------------------------------------------------

    -- i2c_master Signals
    signal   ena            : std_logic := '0';
    constant ADDR           : std_logic_vector(6 downto 0) := "1001010"; -- Slave Address
    signal   rw             : std_logic := '0';
    signal   data_wr        : std_logic_vector(7 downto 0) := (others => '0');
    signal   busy           : std_logic := '0';
    signal   busyprev       : std_logic := '0';
    signal   busycntr       : integer range 0 to 255 := 0;
    signal   data_rd        : std_logic_vector(7 downto 0) := (others => '0');
    signal   ack_error      : std_logic := '0';
    signal   ack_error_prev : std_logic := '1';
    ---------------------------------------------------------------

    --
    signal int_status_reg : std_logic_vector(7 downto 0) := (others => '0');
    signal read_reg       : std_logic_vector(7 downto 0) := (others => '0');
    signal wr_reg         : std_logic_vector(7 downto 0) := (others => '0');
    signal read_done      : std_logic := '0';
    signal read_done_prev : std_logic := '0';
    signal write_done     : std_logic := '0';
    signal read_led_reg   : std_logic := '0';

    -- FIFO Signals
    signal fifo_w     : std_logic := '0';
    signal fifo_r     : std_logic := '0';
    signal data_w     : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
    signal data_r     : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
    signal read_valid : std_logic := '0';
    signal write_ack  : std_logic := '0';
    signal f_empty    : std_logic := '0';
    -- signal uartFIFO_en : STD_LOGIC := '0' ;
    -- END FIFO SIGNALS

    ----------------FSMs----------------
    -- MAIN FSM --

    type states is (RESET, IDLE, Read_Intreg, Read_Intreg_Nxt, Set_TransmitConfig, Set_MainConfig, Read_proxReg, TimeOut);

    signal state : states := IDLE;
    -- FIFO WR FSM --

    type write_states is (IDLE, WRITE, WAIT_W);

    signal state_w : write_states := IDLE;
    -- FIFO RD FSM --

    type read_states is (IDLE, READ, WAIT_R);

    signal state_r : read_states := IDLE;

begin

    FIFO_I : FIFO
        generic map (
            FIFO_DEPTH => 512
        )
        port map (
            CLK        => CLK,
            FIFO_W     => fifo_w,
            FIFO_R     => fifo_r,
            DATA_W     => data_w,
            DATA_R     => data_r,
            READ_VALID => read_valid,
            WRITE_ACK  => write_ack,
            F_EMPTY    => f_empty
        );

    UART_TX_I : UART_TX
        generic map (
            CLK_F    => CLK_F,
            BAUDRATE => 115_200
        )

        port map (
            CLK          => CLK,
            SEND         => send,
            TX_DATA_IN   => tx_data_in,
            TX_DONE_TICK => tx_done_tick,
            TX_DOUT      => TX_DOUT
        );

    I2C_MASTER_I : I2C_MASTER

        generic map (
            INPUT_CLK => CLK_F,
            BUS_CLK   => SCKFREQ
        )
        port map (
            CLK       => CLK,
            RESET_N   => rst_n,
            ENA       => ena,
            ADDR      => ADDR,
            RW        => rw,
            DATA_WR   => data_wr,
            BUSY      => busy,
            DATA_RD   => data_rd,
            ACK_ERROR => ack_error,
            SDA       => SDA,
            SCL       => SCL
        );

    BUT2_STARTINIT : BUTTON_DEBOUNCE
        generic map (
            CLK_F => CLK_F
        )
        port map (
            CLK     => CLK,
            DATAIN  => BUT1,
            DATAOUT => but1_deb
        );

    BUT3_READPROX : BUTTON_DEBOUNCE
        generic map (
            CLK_F => CLK_F
        )
        port map (
            CLK     => CLK,
            DATAIN  => BUT2,
            DATAOUT => but2_deb
        );

    BUT1_RST : BUTTON_DEBOUNCE
        generic map (
            CLK_F => CLK_F
        )
        port map (
            CLK     => CLK,
            DATAIN  => RST,
            DATAOUT => rst_deb
        );

    process (CLK) is
    begin

        if (CLK'event and CLK = '1') then
            READ_LED      <= read_led_reg;
            but1_deb_prev <= but1_deb;
            but2_deb_prev <= but2_deb;
            rst_deb_prev  <= rst_deb;
            -- TX_DONE_TICK_prev <= TX_DONE_TICK ;
            read_done_prev <= read_done;
            ack_error_prev <= ack_error;

            -- if(TX_DONE_TICK_prev = '1' AND TX_DONE_TICK = '0') then
            -- state_en <= '1' ;
            -- end if;

            -- if(read_done_prev = '0' AND read_done = '1') then
            -- READ_LED_reg <= NOT READ_LED_reg ;
            -- end if;

            if (read_done_prev = '1' AND read_done = '0') then
                read_led_reg <= read_led_reg XOR read_led_reg;
            elsif (read_done_prev = '0' AND read_done = '1') then
                read_led_reg <= read_led_reg XOR read_led_reg;
            end if;

            if (ack_error_prev = '0' and ack_error = '1') then
                ACKERR_LED <= '1';
                -- elsif(ack_error_prev = '1' and ack_error = '0') then
                -- ACKERR_LED <= '0' ;
            end if;

            if (rst_deb = '1' AND rst_deb_prev = '0') then -- i2c bus reset not IC
                rst_n      <= '0';
                ACKERR_LED <= '0';
                state      <= RESET;
            end if;

            if (but1_deb_prev = '0' AND but1_deb = '1') then -- start initial configurations
                ACKERR_LED <= '0';
                state      <= READ_INTREG;
            end if;

            if (but2_deb_prev = '0' AND but2_deb = '1') then -- read prox reg consecutively
                ACKERR_LED <= '0';
                state      <= Read_proxReg;
            end if;

            if (state = RESET) then
                RST_LED <= '1';
            else
                RST_LED <= '0';
            end if;

            -- MAIN FSM --
            case state is

                when RESET => -- BUS RESET
                    rst_n <= '0';
                    if (rst_cnt = 5) then
                        rst_cnt <= 0;
                        rst_n   <= '1';
                        state   <= IDLE;
                    elsif (rst_cnt = 2) then
                        rst_cnt <= rst_cnt + 1;
                        rst_n   <= '1';
                    else
                        rst_cnt <= rst_cnt + 1;
                    end if;

                when IDLE =>
                    busyprev  <= busy;
                    ena       <= '0';
                    read_done <= '0';

                when Read_intReg =>
                    busyprev <= busy;
                    if (busyprev = '0' and busy = '1') then
                        busycntr <= busycntr + 1;
                    end if;

                    if (busycntr = 0) then
                        ena     <= '1';
                        rw      <= '0';   -- write operation
                        data_wr <= x"00"; -- int reg adress
                    elsif (busycntr = 1) then
                        rw <= '1';
                    elsif (busycntr = 2) then
                        ena <= '0';
                        if (busy = '0') then
                            read_reg  <= data_rd;
                            read_done <= '1';
                            busycntr  <= 0;
                            state     <= Set_TransmitConfig;
                        end if;
                    end if;

                when Set_TransmitConfig =>
                    busyprev <= busy;
                    if (busyprev = '0' and busy = '1') then
                        busycntr <= busycntr + 1;
                    end if;

                    if (busycntr = 0) then
                        ena     <= '1';
                        rw      <= '0';    -- write operation
                        data_wr <= x"03";  -- reg adress
                    elsif (busycntr = 1) then
                        data_wr <= x"06";  -- data to be written
                    elsif (busycntr = 2) then
                        ena <= '0';
                        if (busy = '0') then
                            wr_reg     <= x"60";
                            write_done <= '1';
                            busycntr   <= 0;
                            state      <= Set_MainConfig;
                        end if;
                    end if;

                when Set_MainConfig =>
                    busyprev <= busy;
                    if (busyprev = '0' and busy = '1') then
                        busycntr <= busycntr + 1;
                    end if;

                    if (busycntr = 0) then
                        ena     <= '1';
                        rw      <= '0';    -- write operation
                        data_wr <= x"01";  -- reg adress
                    elsif (busycntr = 1) then
                        data_wr <= x"34";  -- data to be written
                    elsif (busycntr = 2) then
                        ena <= '0';
                        if (busy = '0') then
                            wr_reg     <= x"61";
                            write_done <= '1';
                            busycntr   <= 0;
                            state      <= IDLE;
                        end if;
                    end if;

                when Read_proxReg =>
                    busyprev <= busy;
                    if (busyprev = '0' and busy = '1') then
                        busycntr <= busycntr + 1;
                    end if;

                    if (busycntr = 0) then
                        ena     <= '1';
                        rw      <= '0';    -- write operation
                        data_wr <= x"16";  -- int reg adress
                    elsif (busycntr = 1) then
                        rw <= '1';
                    elsif (busycntr = 2) then
                        ena <= '0';
                        if (busy = '0') then
                            read_reg  <= data_rd;
                            read_done <= '1';
                            busycntr  <= 0;
                            state     <= IDLE;
                        end if;
                    end if;

                when TimeOut =>
                    if (cntr1s = LIM1S - 1) then
                        cntr1s <= 0;
                        state  <= Read_proxReg;
                    else
                        cntr1s <= cntr1s + 1;
                    end if;

                    -- end if ;

                when OTHERS =>
                    NULL;

            end case;

            -- MAIN FSM END --

            -- FIFO WR FSM --
            case(state_w) is

                when IDLE =>
                    if (read_done = '1') then -- or trig = '1') then
                        data_w    <= read_reg;
                        read_done <= '0';
                        state_w   <= WRITE;
                    elsif (write_done = '1') then
                        write_done <= '0';
                        data_w     <= wr_reg;
                        state_w    <= WRITE;
                    else
                        state_w <= IDLE;
                    end if;

                when WRITE =>
                    fifo_w  <= '1';
                    state_w <= WAIT_W;

                when WAIT_W =>
                    fifo_w <= '0';
                    if (write_ack = '1') then
                        state_w <= IDLE;
                    else
                        state_w <= WAIT_W;
                    end if;

            end case;

            -- FIFO WR FSM  END --

            -- FIFO RD FSM + UART_TX --
            case(state_r) is

                when IDLE =>
                    send <= '0';
                    if (write_ack = '1') then
                        fifo_r  <= '1';
                        state_r <= READ;
                    else
                        fifo_r <= '0';
                    end if;

                when READ =>
                    fifo_r <= '0';
                    if (read_valid ='1') then
                        tx_data_in <= data_r;
                        send       <= '1';
                        state_r    <= WAIT_R;
                    end if;

                when WAIT_R =>
                    send <= '0';
                    if (tx_done_tick = '1') then
                        if (f_empty = '1') then
                            state_r <= IDLE;
                            fifo_r  <= '0';
                        else
                            state_r <= READ;
                            fifo_r  <= '1';
                        end if;
                    else
                        fifo_r  <= '0';
                        state_r <= WAIT_R;
                    end if;

            end case;

            -- FIFO FSM END --
        end if;

    end process;

end architecture BEHAVIORAL;
