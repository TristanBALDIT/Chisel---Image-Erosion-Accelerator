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
    cross_center_x * 20.U + cross_center_y,               // center
    (cross_center_x - 1.U) * 20.U + cross_center_y,       // top
    (cross_center_x + 1.U) * 20.U + cross_center_y,       // bottom
    cross_center_x       * 20.U + (cross_center_y - 1.U), // left
    cross_center_x       * 20.U + (cross_center_y + 1.U)  // right
  ))

  val cross_addresses_valid = VecInit(Seq(
    true.B,
    cross_center_y  > 0.U,
    cross_center_y < 19.U,
    cross_center_x > 0.U,
    cross_center_x < 19.U
  ))

  val write_idx = RegInit(0.U(4.W))

  val idle :: readCenter :: readCross :: writeCross :: Nil = Enum(2)

  val addressRead = RegInit(0.U(16.W))
  val addressWrite = RegInit(0.U(16.W))

  val diag_buffers = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(7)(VecInit(Seq.fill(2)(0.U(8.W))))))))

  val top = RegInit(0.U(1.W))
  val bottom = top + 1.U

  val buffer_idx = RegInit(8.U(6.W))
  val initial_buffer_idx = RegInit(8.U(6.W))

  val cross_buffer = RegInit(VecInit(Seq.fill(5)(0.U(8.W))))

  io.done := false.B
  io.writeEnable := false.B
  io.address := Mux(io.writeEnable, addressWrite, addressRead)

  io.dataWrite := cross_buffer(write_idx)

  switch(state){

    is(idle) {
      when(io.start) {
        state := readCenter
        addressRead := 0.U
        addressWrite := 400.U

        cross_center_x := 16.U
        cross_center_y := 0.U

        buffer_idx := 8.U
        initial_buffer_idx := 8.U

        top := 0.U
      }
    }

    is(readCenter){
      when(io.dataRead(7,0) === 0.U){
        cross_buffer := VecInit(Seq.fill(5)(0.U))
        state := writeCross

        diag_buffers(bottom)(buffer_idx)(0) := 1.U
        diag_buffers(bottom)(buffer_idx)(1) := 1.U
      }.otherwise{
        cross_buffer(0) := io.dataRead(7,0)
        state := readCross
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
      }.otherwise{
        write_idx := write_idx + 2.U
      }
    }
  }
}
