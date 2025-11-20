import chisel3._
import chisel3.util._

class AcceleratorOpti3 {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  val state = RegInit(idle)

  val cross_center_x = RegInit(0.U(6.W))
  val cross_center_y = RegInit(0.U(6.W))

  val center_address = cross_center_x * 20.U + cross_center_y

  val cross_addresses = VecInit(Seq(
    cross_center_x * 20.U + cross_center_y,                 // center
    (cross_center_x + 1.U) * 20.U + cross_center_y,         // bottom
    (cross_center_x - 1.U) * 20.U + cross_center_y,         // top
    cross_center_x       * 20.U + (cross_center_y + 1.U),   // right
    cross_center_x       * 20.U + (cross_center_y - 1.U)    // left

  ))

  val cross_addresses_valid = VecInit(Seq(
    true.B,
    cross_center_y  > 0.U,
    cross_center_y < 19.U,
    cross_center_x > 0.U,
    cross_center_x < 19.U
  ))

  val write_idx = RegInit(0.U(4.W))
  val read_idx = RegInit(0.U(4.W))
  val readOuter_idx = RegInit(0.U(4.W))

  val idle :: readCenter :: readInnerCross :: readOuterCross :: writeCross :: Nil = Enum(5)

  val bot_diag_buffers = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(0.U(8.W))))))))
  val top_actual_diag_buffer = RegInit(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(0.U(8.W))))))

  val actual = RegInit(0.U(1.W))
  val previous = actual + 1.U

  val buffer_idx = RegInit(8.U(6.W))
  val initial_buffer_idx = RegInit(8.U(6.W))

  val cross_buffer = RegInit(VecInit(Seq.fill(5)(0.U(8.W))))

  val addressWrite = cross_addresses(write_idx)
  val addressRead = cross_addresses(read_idx)

  io.done := false.B
  io.writeEnable := false.B
  io.address := Mux(io.writeEnable, addressWrite, addressRead)

  io.dataWrite := cross_buffer(write_idx)

  switch(state){

    is(idle) {
      when(io.start) {
        state := readCenter

        cross_center_x := 16.U
        cross_center_y := 0.U

        buffer_idx := 8.U
        initial_buffer_idx := 8.U

        actual := 0.U

        read_idx := 0.U
        write_idx := 0.U
      }
    }

    is(readCenter){
      // BLACK CENTER
      when(io.dataRead(7,0) === 0.U){
        cross_buffer := VecInit(Seq.fill(5)(0.U))
        state := writeCross

        bot_diag_buffers(actual)(buffer_idx)(0) := 1.U
        bot_diag_buffers(actual)(buffer_idx)(1) := 1.U

      // WHITE CENTER : GO READ INNER CROSS
      }.otherwise{
        cross_buffer(0) := io.dataRead(7,0)
        state := readInnerCross
        read_idx := read_idx + 1.U                    // FIRST : BOT
      }
    }

    is(writeCross){
      // WRITING PROCESS
      io.writeEnable := true.B


      // EXIT STATE
      when(write_idx === 5.U || (write_idx === 4.U && !cross_addresses_valid(write_idx+1.U))){
        state := readCenter
        write_idx := 0.U
        //CONTINUE DIAGONAL
        when(cross_center_x  < 18.U && cross_center_y < 19.U){
          cross_center_x := cross_center_x + 2.U
          cross_center_y := cross_center_y + 2.U

        // NEW DIAGONAL
        }.otherwise{
          // BIGGER DIAGONAL
          when(cross_center_x - 5.U >= 0.U){
            cross_center_x := cross_center_x - 5.U
            buffer_idx := initial_buffer_idx - 1.U
            initial_buffer_idx := initial_buffer_idx - 2.U
          // SAME SIZE
          }.elsewhen(cross_center_x - 1.U >= 0.U && cross_center_y + 2.U < 20.U){
            cross_center_x := cross_center_x - 1.U
            cross_center_y := cross_center_y + 2.U
          // SMALLER DIAG
          }.otherwise {
            cross_center_x := cross_center_x + 1.U
            cross_center_y := cross_center_y + 3.U
            buffer_idx := initial_buffer_idx + 1.U
          }
        }

      // UPDATE WRITING IDX FOR NEXT CYCLE
      }.elsewhen(cross_addresses_valid(write_idx+1.U)){
        write_idx := write_idx + 1.U
      }.elsewhen(cross_addresses_valid(write_idx+2.U)){
        write_idx := write_idx + 2.U
      }.otherwise {
        write_idx := write_idx + 3.U
      }
    }

    is(readInnerCross){

      cross_buffer(read_idx) := io.dataRead(7,0)

      when(io.dataRead(7,0) === 0.U){
        cross_buffer(0) := 0.U
      }

      // EXIT STATE
      when(read_idx === 4.U){

        when(readOuter_idx > 0.U){
          state := readOuterCross
        }.otherwise {
          state := writeCross
        }

      // WHEN NEXT IS TOP AND TOP ALREADY KNOWN SKIP TOP
      }.elsewhen(read_idx === 1.U && cross_addresses_valid(read_idx+1.U) && top_actual_diag_buffer(buffer_idx)(0) =/= 1.U){

        cross_buffer(read_idx + 1.U) := top_actual_diag_buffer(buffer_idx)(0)

        // SET CENTER TO BLACK IS BLACK LOADED
        when(top_actual_diag_buffer(buffer_idx)(1) === 0.U){
          cross_buffer(0) := 0.U
        }

        // SKIP ALSO RIGHT
        when(cross_addresses_valid(read_idx+1.U) && top_actual_diag_buffer(buffer_idx)(0) =/= 1.U){
          cross_buffer(read_idx + 2.U) := top_actual_diag_buffer(buffer_idx)(1)
          read_idx := read_idx + 3.U

          // SET CENTER TO BLACK IS BLACK LOADED
          when(top_actual_diag_buffer(buffer_idx)(1) === 0.U){
            cross_buffer(0) := 0.U
          }
        }.otherwise {
          read_idx := read_idx + 2.U
        }
      // WHEN NEXT IS RIGHT AND RIGHT ALREADY KNOWN SKIP TOP
      }.elsewhen(read_idx === 2.U && cross_addresses_valid(read_idx+1.U) && top_actual_diag_buffer(buffer_idx)(0) =/= 1.U){
        read_idx := read_idx + 2.U
        cross_buffer(read_idx + 1.U) := top_actual_diag_buffer(buffer_idx)(1)

        // SET CENTER TO BLACK IS BLACK LOADED
        when(top_actual_diag_buffer(buffer_idx)(1) === 0.U){
          cross_buffer(0) := 0.U
        }
      // NEXT PIXEL OUTSIDE IMAGE : SKIP
      }.elsewhen(!cross_addresses_valid(read_idx+1.U)) {
        // NEXT NEWT PIXEL OUTSIDE IMAGE : SKIP
        when(!cross_addresses_valid(read_idx+2.U)){
          read_idx := read_idx+3.U
        }
        read_idx := read_idx+2.U
      }.otherwise {
        read_idx := read_idx+1.U
      }
    }
  }
}
