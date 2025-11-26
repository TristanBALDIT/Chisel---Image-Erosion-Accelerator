import chisel3._
import chisel3.util._

class AcceleratorOpti3 extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  val idle :: setBorder :: readCenter :: readInnerCross :: readOuterCross :: writeCross :: writeCorners :: done :: Nil = Enum(8)

  val state = RegInit(idle)

  val cross_center_x = RegInit(0.U(6.W))
  val cross_center_y = RegInit(0.U(6.W))
  val initial_x = RegInit(0.U(6.W))
  val initial_y = RegInit(0.U(6.W))

  val center_address = cross_center_x * 20.U + cross_center_y

  val write_idx = RegInit(0.U(4.W))
  val read_idx = RegInit(0.U(4.W))

  val previous_cross_buffers =  RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(1.U(8.W))))))
  val next_cross_buffers = RegInit(VecInit(Seq.fill(2)(1.U(8.W))))

  val actual_cross = RegInit(0.U(1.W))
  val previous_cross = !actual_cross

  val bot_diag_buffers = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(1.U(8.W))))))))
  val top_actual_diag_buffer = RegInit(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(1.U(8.W))))))

  val actual_line = RegInit(0.U(1.W))
  val previous_line = !actual_line

  val buffer_idx = RegInit(0.U(6.W))
  val initial_buffer_idx = RegInit(8.U(6.W))

  val cross_buffer = RegInit(VecInit(Seq.fill(5)(0.U(8.W))))

  val cornerWrote = RegInit(VecInit(Seq.fill(4)(0.B)))

  val corner_addresses = VecInit(Seq(
    0.U,
    19.U,
    380.U,
    399.U
  ))


  val allCornerWrote = cornerWrote(0) && cornerWrote(1) && cornerWrote(2) && cornerWrote(3)

  val cross_addresses = VecInit(Seq(
    cross_center_y * 20.U + cross_center_x,                 // center
    (cross_center_y + 1.U) * 20.U + cross_center_x,         // bottom
    (cross_center_y - 1.U) * 20.U + cross_center_x,         // top
    cross_center_y  * 20.U + (cross_center_x + 1.U),        // right
    cross_center_y  * 20.U + (cross_center_x - 1.U),        // left
    (cross_center_y + 1.U) * 20.U + (cross_center_x - 1.U), // bottom left
    (cross_center_y - 1.U) * 20.U + (cross_center_x - 1.U), // top left
    (cross_center_y - 1.U) * 20.U + (cross_center_x + 1.U), // top right
    (cross_center_y + 1.U) * 20.U + (cross_center_x + 1.U), // bottom right
    (cross_center_y + 2.U) * 20.U + cross_center_x,         // bottom bottom
    cross_center_y * 20.U + (cross_center_x - 2.U),         // left left
    (cross_center_y - 2.U) * 20.U + cross_center_x,         // top top
    cross_center_y * 20.U + (cross_center_x + 2.U)          // right right
  ))

  val cross_addresses_valid = VecInit(Seq(
    true.B,
    cross_center_y < 19.U,
    cross_center_y  > 0.U,
    cross_center_x < 19.U,
    cross_center_x > 0.U,
    cross_center_y < 19.U && cross_center_x > 0.U,
    cross_center_y < 19.U && cross_center_x < 19.U,
    cross_center_y < 18.U,
    cross_center_y  > 0.U && cross_center_x > 0.U,
    cross_center_y  > 0.U && cross_center_x < 19.U,
    cross_center_y  > 1.U,
    cross_center_x < 18.U,
    cross_center_x > 1.U
  ))

  val outer_usefull = VecInit(Seq(
    cross_buffer(1) === 255.U || cross_buffer(4) === 255.U || (io.dataRead(7,0) === 255.U && read_idx === 4.U),
    (cross_buffer(2) === 255.U || (cross_buffer(4) === 255.U && !(io.dataRead(7,0) === 0.U && read_idx === 5.U)) || (io.dataRead(7,0) === 255.U && read_idx === 4.U)) && previous_cross_buffers(previous_cross)(0) === 1.U ,
    ((cross_buffer(2) === 255.U && !(io.dataRead(7,0) === 0.U && read_idx === 6.U)) || cross_buffer(3) === 255.U) && bot_diag_buffers(previous_line)(buffer_idx)(1) === 1.U,
    (cross_buffer(1) === 255.U && !(io.dataRead(7,0) === 0.U && read_idx === 7.U)) || (cross_buffer(3) === 255.U && !(io.dataRead(7,0) === 0.U && read_idx === 7.U)),
    cross_buffer(1) === 255.U && !(io.dataRead(7,0) === 0.U && (read_idx === 5.U || read_idx === 8.U)),
    cross_buffer(4) === 255.U && previous_cross_buffers(previous_cross)(1) === 1.U && !(io.dataRead(5,0) === 0.U && (read_idx === 6.U || read_idx === 6.U)),
    cross_buffer(2) === 255.U && bot_diag_buffers(previous_line)(buffer_idx)(0) === 1.U && !(io.dataRead(7,0) === 0.U && (read_idx === 6.U || read_idx === 7.U)),
    cross_buffer(3) === 255.U && top_actual_diag_buffer(buffer_idx + 1.U)(0) === 1.U && !(io.dataRead(7,0) === 0.U && (read_idx === 7.U || read_idx === 8.U))
  ))

  io.done := false.B
  io.writeEnable := false.B

  val addressWrite = Mux(state === writeCorners, corner_addresses(write_idx), cross_addresses(write_idx))
  val addressRead = cross_addresses(read_idx)
  io.address := Mux(io.writeEnable, addressWrite + 400.U, addressRead)

  io.dataWrite := Mux(state===setBorder || state===writeCorners, 0.U, cross_buffer(write_idx))

  when(io.writeEnable && addressWrite === 0.U){
    cornerWrote(0) := true.B
  }
  when(io.writeEnable && addressWrite === 19.U){
    cornerWrote(1) := true.B
  }
  when(io.writeEnable && addressWrite === 380.U){
    cornerWrote(2) := true.B
  }
  when(io.writeEnable && addressWrite === 399.U){
    cornerWrote(3) := true.B
  }

  switch(state){

    is(idle) {
      when(io.start) {
        state := readCenter

        cross_center_x := 16.U
        cross_center_y := 0.U
        initial_x := 16.U
        initial_y := 0.U

        buffer_idx := 6.U
        initial_buffer_idx := 6.U

        actual_line := 0.U
        actual_cross := 0.U

        read_idx := 0.U
        write_idx := 0.U
      }
    }

    is(readCenter){
      // BLACK CENTER
      when(io.dataRead(7,0) === 0.U){
        cross_buffer := VecInit(Seq.fill(5)(0.U))
        state := writeCross

        bot_diag_buffers(actual_line)(buffer_idx)(0) := 1.U
        bot_diag_buffers(actual_line)(buffer_idx)(1) := 1.U
        previous_cross_buffers(actual_cross)(0) := 1.U
        previous_cross_buffers(actual_cross)(1) := 1.U

      // WHITE CENTER : GO READ INNER CROSS
      }.otherwise{
        cross_buffer(0) := io.dataRead(7,0)
        state := readInnerCross
        // IF BOT VALID : FIRST BOT
        when(cross_addresses_valid(1)){
          read_idx := read_idx + 1.U
        // ELSE GO TOP
        }.otherwise {
          read_idx := read_idx + 2.U
        }
      }
    }

    is(writeCross){
      // WRITING PROCESS
      io.writeEnable := true.B

      // EXIT STATE
      when(write_idx === 4.U && (cross_center_x === 1.U || cross_center_x === 18.U || cross_center_y === 1.U || cross_center_y === 18.U)){
        state := setBorder
        when(cross_center_y === 18.U){
          write_idx := 5.U
        }.elsewhen(cross_center_x === 1.U){
          write_idx := 6.U
        }.elsewhen(cross_center_y === 1.U){
          write_idx := 7.U
        }.otherwise{
          write_idx := 8.U
        }
      }.elsewhen(write_idx === 4.U || (write_idx === 3.U && !cross_addresses_valid(write_idx+1.U))){
        state := readCenter
        write_idx := 0.U
        //CONTINUE DIAGONAL
        when(cross_center_x  < 18.U && cross_center_y < 19.U){
          cross_center_x := cross_center_x + 2.U
          cross_center_y := cross_center_y + 1.U
          buffer_idx := buffer_idx + 1.U
          actual_cross := !actual_cross

        // NEW DIAGONAL
        }.otherwise{

          //RESET NEXT BUFFER AND PREVIOUS
          next_cross_buffers := VecInit(Seq.fill(2)(1.U(8.W)))
          previous_cross_buffers := VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(1.U(8.W)))))
          actual_cross := 0.U

          // UPDATE BOT LINE BUFFERS
          bot_diag_buffers(previous_line) := VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(1.U(8.W)))))
          actual_line := previous_line

          // BIGGER DIAGONAL
          when(initial_x >= 5.U){
            initial_x := initial_x - 5.U
            cross_center_x := initial_x - 5.U
            cross_center_y := initial_y
            buffer_idx := initial_buffer_idx - 2.U
            initial_buffer_idx := initial_buffer_idx - 2.U
          // SAME SIZE
          }.elsewhen(initial_x  >= 1.U && initial_y + 2.U < 20.U){
            initial_x := initial_x - 1.U
            initial_y := initial_y + 2.U
            cross_center_x := initial_x - 1.U
            cross_center_y := initial_y + 2.U
            buffer_idx := initial_buffer_idx
          // SMALLER DIAG
          }.elsewhen(initial_y + 3.U < 20.U) {
            initial_x := initial_x + 1.U
            initial_y := initial_y + 3.U
            cross_center_x := initial_x + 1.U
            cross_center_y := initial_y + 3.U
            buffer_idx := initial_buffer_idx + 1.U
            initial_buffer_idx := initial_buffer_idx + 1.U
          }.otherwise {
            when(allCornerWrote){
              state := done
            }.otherwise{
              state := writeCorners
              when(!cornerWrote(0)){
                write_idx := 0.U
              }.elsewhen(!cornerWrote(1)){
                write_idx := 1.U
              }.elsewhen(!cornerWrote(2)){
                write_idx := 2.U
              }.otherwise{
                write_idx := 3.U
              }
            }
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

    is(readInnerCross) {

      // ------------------------------
      // Helpers
      // ------------------------------

      def isKnown(x: UInt) = x =/= 1.U

      def isBlack(x: UInt) = x === 0.U

      def setCross(idx: UInt, value: UInt) = {
        cross_buffer(idx) := value
        when(isBlack(value)) {
          cross_buffer(0) := 0.U
        }
      }

      val exitIdx = WireDefault(0.U)
      val exitState = WireDefault(0.U)

      when(outer_usefull(0) && cross_addresses_valid(5)) {
        exitState := readOuterCross
        exitIdx := 5.U
      }.elsewhen(outer_usefull(1) && cross_addresses_valid(6)) {
        exitState := readOuterCross
        exitIdx := 6.U
      }.elsewhen(outer_usefull(2) && cross_addresses_valid(7)) {
        exitState := readOuterCross
        exitIdx := 7.U
      }.elsewhen(outer_usefull(3) && cross_addresses_valid(8)) {
        exitState := readOuterCross
        exitIdx := 8.U
      }.otherwise {
        exitState := writeCross
        exitIdx := 0.U
      }

      val top_top = top_actual_diag_buffer(buffer_idx)(0)
      val top_right = top_actual_diag_buffer(buffer_idx)(1)
      val next_top = next_cross_buffers(0)
      val next_left = next_cross_buffers(1)


      val cross_known = VecInit(Seq(
        false.B,
        false.B,
        isKnown(top_top) || isKnown(next_top),
        isKnown(top_right),
        isKnown(next_left)
      ))

      // ------------------------------
      // State & Idx transition & READ
      // ------------------------------

      setCross(read_idx, io.dataRead(7, 0))

      when((cross_addresses_valid(read_idx + 1.U) && !cross_known(read_idx + 1.U)) && read_idx < 4.U) {
        read_idx := read_idx + 1.U
      }.elsewhen((cross_addresses_valid(read_idx + 2.U) && !cross_known(read_idx + 2.U)) && read_idx < 3.U) {
        read_idx := read_idx + 2.U
      }.elsewhen((cross_addresses_valid(read_idx + 3.U) && !cross_known(read_idx + 3.U)) && read_idx < 2.U) {
        read_idx := read_idx + 3.U
      }.otherwise {
        state := exitState
        read_idx := exitIdx
        // RESET BUFFER VALUES
        next_cross_buffers := VecInit(Seq.fill(2)(1.U(8.W)))
        top_actual_diag_buffer(buffer_idx) := VecInit(Seq.fill(2)(1.U(8.W)))
      }

      // ------------------------------
      // Buffer writes by read_idx
      // ------------------------------

      switch(read_idx) {
        is(1.U) {
          bot_diag_buffers(actual_line)(buffer_idx)(1) := io.dataRead(7, 0)
          previous_cross_buffers(actual_cross)(1) := io.dataRead(7, 0)
        }
        is(3.U) {
          previous_cross_buffers(actual_cross)(0) := io.dataRead(7, 0)
        }
        is(4.U) {
          bot_diag_buffers(actual_line)(buffer_idx)(0) := io.dataRead(7, 0)
        }
      }

      // ------------------------------
      // Buffer read
      // ------------------------------

      when(cross_known(2.U)) {
        when(isKnown(top_top)) {
          setCross(2.U, top_top)
        }.otherwise {
          setCross(2.U, next_top)
        }
      }

      when(cross_known(3.U)) {
        setCross(3.U, top_right)
        previous_cross_buffers(actual_cross)(0) := top_right
      }

      when(cross_known(4.U)) {
        setCross(4.U, next_left)
        bot_diag_buffers(actual_line)(buffer_idx)(0) := next_left
      }
    }




    is(readOuterCross){

      // READ BUFFERED VALUES
      when(previous_cross_buffers(previous_cross)(0) === 0.U){
        cross_buffer(2) := 0.U
        cross_buffer(4) := 0.U
      }
      when(bot_diag_buffers(previous_line)(buffer_idx)(1) === 0.U){
        cross_buffer(2) := 0.U
        cross_buffer(3) := 0.U
      }
      when(previous_cross_buffers(previous_cross)(1) === 0.U){
        cross_buffer(4) := 0.U
      }
      when(bot_diag_buffers(previous_line)(buffer_idx)(0) === 0.U){
        cross_buffer(2) := 0.U
      }
      when(top_actual_diag_buffer(buffer_idx + 1.U)(0) === 0.U){
        cross_buffer(3) := 0.U
      }

      // PIXEL BLACK : UPDATE CROSS BUFFER AND OTHER BUFFER
      switch(read_idx) {
        is(5.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(1) := 0.U
            cross_buffer(4) := 0.U
          }
          top_actual_diag_buffer(buffer_idx)(0) := io.dataRead(7,0)
        }
        is(6.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(2) := 0.U
            cross_buffer(4) := 0.U
          }
        }
        is(7.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(2) := 0.U
            cross_buffer(3) := 0.U
          }
        }
        is(8.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(1) := 0.U
            cross_buffer(3) := 0.U
          }
          next_cross_buffers(1) := io.dataRead(7,0)
        }
        is(9.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(1) := 0.U
          }
          top_actual_diag_buffer(buffer_idx)(1) := io.dataRead(7,0)
        }
        is(10.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(4) := 0.U
          }
        }
        is(11.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(2) := 0.U
          }
        }
        is(12.U) {
          when(io.dataRead(7,0) === 0.U) {
            cross_buffer(3) := 0.U
          }
          next_cross_buffers(0) := io.dataRead(7,0)
        }
      }



      //EXIT
      when(read_idx === 12.U){
        state := writeCross
        read_idx := 0.U
      }.elsewhen(!cross_addresses_valid(read_idx+1.U) || !outer_usefull(read_idx+1.U - 5.U) && read_idx < 11.U){
        when(!cross_addresses_valid(read_idx+2.U) || !outer_usefull(read_idx+2.U - 5.U) && read_idx < 10.U){
          when(!cross_addresses_valid(read_idx+3.U) || !outer_usefull(read_idx+3.U - 5.U) && read_idx < 9.U){
            when(!cross_addresses_valid(read_idx+4.U) || !outer_usefull(read_idx+4.U - 5.U) && read_idx < 8.U){
              state := writeCross
              read_idx := 0.U
            }.otherwise {
              read_idx := read_idx + 4.U
            }
          }.otherwise{
            read_idx := read_idx + 3.U
          }
        }.otherwise{
          read_idx := read_idx + 2.U
        }
      }.otherwise {
        read_idx := read_idx + 1.U
      }
    }

    is(setBorder){
      io.writeEnable := true.B

      when(cross_center_x === 1.U && write_idx < 6.U){
        write_idx := 6.U
      }.elsewhen(cross_center_y === 1.U && write_idx < 7.U){
        write_idx := 7.U
      }.elsewhen(cross_center_x === 18.U && write_idx < 8.U){
        write_idx := 8.U
      }.otherwise{
        state := readCenter
        write_idx := 0.U
        //CONTINUE DIAGONAL
        when(cross_center_x  < 18.U && cross_center_y < 19.U){
          cross_center_x := cross_center_x + 2.U
          cross_center_y := cross_center_y + 1.U
          buffer_idx := buffer_idx + 1.U
          actual_cross := !actual_cross

          // NEW DIAGONAL
        }.otherwise{

          //RESET NEXT BUFFER AND PREVIOUS
          next_cross_buffers := VecInit(Seq.fill(2)(1.U(8.W)))
          previous_cross_buffers := VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(1.U(8.W)))))
          actual_cross := 0.U

          // UPDATE BOT LINE BUFFERS
          bot_diag_buffers(previous_line) := VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(1.U(8.W)))))
          actual_line := previous_line

          // BIGGER DIAGONAL
          when(initial_x >= 5.U){
            initial_x := initial_x - 5.U
            cross_center_x := initial_x - 5.U
            cross_center_y := initial_y
            buffer_idx := initial_buffer_idx - 2.U
            initial_buffer_idx := initial_buffer_idx - 2.U
            // SAME SIZE
          }.elsewhen(initial_x  >= 1.U && initial_y + 2.U < 20.U){
            initial_x := initial_x - 1.U
            initial_y := initial_y + 2.U
            cross_center_x := initial_x - 1.U
            cross_center_y := initial_y + 2.U
            buffer_idx := initial_buffer_idx
            // SMALLER DIAG
          }.elsewhen(initial_y + 3.U < 20.U) {
            initial_x := initial_x + 1.U
            initial_y := initial_y + 3.U
            cross_center_x := initial_x + 1.U
            cross_center_y := initial_y + 3.U
            buffer_idx := initial_buffer_idx + 1.U
            initial_buffer_idx := initial_buffer_idx + 1.U
          }.otherwise {
            when(allCornerWrote){
              state := done
            }.otherwise{
              state := writeCorners
              when(!cornerWrote(0)){
                write_idx := 0.U
              }.elsewhen(!cornerWrote(1)){
                write_idx := 1.U
              }.elsewhen(!cornerWrote(2)){
                write_idx := 2.U
              }.otherwise{
                write_idx := 3.U
              }
            }
          }
        }
      }
    }

    is(done){
      io.done := true.B
    }

    is(writeCorners){
      io.writeEnable := true.B

      when(!cornerWrote(1) && write_idx =/= 1.U){
        write_idx := 1.U
      }.elsewhen(!cornerWrote(2) && write_idx =/= 2.U){
        write_idx := 2.U
      }.elsewhen(!cornerWrote(3) && write_idx =/= 3.U){
        write_idx := 3.U
      }.otherwise{
        state := done
      }
    }
  }
}
