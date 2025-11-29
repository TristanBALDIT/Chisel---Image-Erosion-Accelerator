import chisel3._
import chisel3.util._

class AcceleratorOpti3 extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done  = Output(Bool())

    val address     = Output(UInt(16.W))
    val dataRead    = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite   = Output(UInt(32.W))

  })

  object State {
    val idle :: setBorder :: readCenter :: readInnerCross :: readOuterCross :: writeCross :: writeCorners :: done :: Nil = Enum(8)
  }

  object Color {
    val white   = 255.U(8.W)
    val black   = 0.U(8.W)
    val unknown = 1.U(8.W)
  }

  val data_read = io.dataRead(7, 0)

  val image_size = 20.U

  val state = RegInit(State.idle)

  val cross_center_x = RegInit(0.U(6.W))
  val cross_center_y = RegInit(0.U(6.W))
  val initial_x      = RegInit(0.U(6.W))
  val initial_y      = RegInit(0.U(6.W))

  val write_idx = RegInit(0.U(4.W))
  val read_idx  = RegInit(0.U(4.W))

  val previous_cross_buffers = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(Color.unknown)))))
  val next_cross_buffers     = RegInit(VecInit(Seq.fill(2)(Color.unknown)))

  val actual_cross   = RegInit(false.B)
  val previous_cross = !actual_cross

  val bot_diag_buffers       = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(Color.unknown)))))))
  val top_actual_diag_buffer = RegInit(VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(Color.unknown)))))

  val actual_line   = RegInit(false.B)
  val previous_line = !actual_line

  val buffer_idx         = RegInit(0.U(6.W))
  val initial_buffer_idx = RegInit(8.U(6.W))

  val cross_buffer = RegInit(VecInit(Seq.fill(5)(Color.black)))

  val cornerWrote    = RegInit(VecInit(Seq.fill(4)(false.B)))
  val allCornerWrote = cornerWrote(0) && cornerWrote(1) && cornerWrote(2) && cornerWrote(3)

  val corner_addresses = VecInit(
    Seq(
      0.U,
      image_size - 1.U,
      image_size * (image_size - 1.U),
      image_size * image_size - 1.U
    )
  )

  val cross_addresses = VecInit(
    Seq(
      cross_center_y * image_size + cross_center_x,                 // center
      (cross_center_y + 1.U) * image_size + cross_center_x,         // bottom
      (cross_center_y - 1.U) * image_size + cross_center_x,         // top
      cross_center_y * image_size + (cross_center_x + 1.U),         // right
      cross_center_y * image_size + (cross_center_x - 1.U),         // left
      (cross_center_y + 1.U) * image_size + (cross_center_x - 1.U), // bottom left
      (cross_center_y - 1.U) * image_size + (cross_center_x - 1.U), // top left
      (cross_center_y - 1.U) * image_size + (cross_center_x + 1.U), // top right
      (cross_center_y + 1.U) * image_size + (cross_center_x + 1.U), // bottom right
      (cross_center_y + 2.U) * image_size + cross_center_x,         // bottom bottom
      cross_center_y * image_size + (cross_center_x - 2.U),         // left left
      (cross_center_y - 2.U) * image_size + cross_center_x,         // top top
      cross_center_y * image_size + (cross_center_x + 2.U)          // right right
    )
  )

  val cross_addresses_valid = VecInit(
    Seq(
      true.B,                                                                   // center
      cross_center_y < image_size - 1.U,                                        // bottom
      cross_center_y > 0.U,                                                     // top
      cross_center_x < image_size - 1.U,                                        // right
      cross_center_x > 0.U,                                                     // left
      cross_center_y < image_size - 1.U && cross_center_x > 0.U,                // bottom left
      cross_center_y > 0.U && cross_center_x > 0.U,                             // top left
      cross_center_y > 0.U && cross_center_x < image_size - 1.U,                // top right
      cross_center_y < image_size - 1.U && cross_center_x < image_size - 1.U,   // bottom left
      cross_center_y < image_size - 2.U,                                        // bot bot
      cross_center_x > 1.U,                                                     // left left
      cross_center_y > 1.U,                                                     // top top
      cross_center_x < image_size - 2.U                                         // right right
    )
  )

  val outer_useful = VecInit(
    Seq(
      cross_buffer(1) === Color.white
        || cross_buffer(4) === Color.white
        || (data_read === Color.white && read_idx === 4.U),
      (cross_buffer(2) === Color.white || (cross_buffer(4) === Color.white && !(data_read === Color.black && read_idx === 5.U)) || (data_read === Color.white && read_idx === 4.U))
        && previous_cross_buffers(previous_cross)(0) === Color.unknown,
      ((cross_buffer(2) === Color.white && !(data_read === Color.black && read_idx === 6.U)) || cross_buffer(3) === Color.white)
        && bot_diag_buffers(previous_line)(buffer_idx)(1) === Color.unknown,
      (cross_buffer(1) === Color.white && !(data_read === Color.black && read_idx === 7.U))
        || (cross_buffer(3) === Color.white && !(data_read === Color.black && read_idx === 7.U)),
      cross_buffer(1) === Color.white
        && !(data_read === Color.black && (read_idx === 5.U || read_idx === 8.U)),
      cross_buffer(4) === Color.white
        && previous_cross_buffers(previous_cross)(1) === Color.unknown
        && !(data_read === Color.black && (read_idx === 6.U || read_idx === 6.U)),
      cross_buffer(2) === Color.white
        && bot_diag_buffers(previous_line)(buffer_idx)(0) === Color.unknown
        && !(data_read === Color.black && (read_idx === 6.U || read_idx === 7.U)),
      cross_buffer(3) === Color.white
        && top_actual_diag_buffer(buffer_idx + 1.U)(0) === Color.unknown
        && !(data_read === Color.black && (read_idx === 7.U || read_idx === 8.U))
    )
  )

  val valid_as_center = VecInit(
    cross_addresses_valid(1) && cross_addresses_valid(2) && cross_addresses_valid(3) && cross_addresses_valid(4),
    cross_addresses_valid(5) && cross_addresses_valid(8) && cross_addresses_valid(9),
    cross_addresses_valid(6) && cross_addresses_valid(7) && cross_addresses_valid(11),
    cross_addresses_valid(7) && cross_addresses_valid(8) && cross_addresses_valid(12),
    cross_addresses_valid(5) && cross_addresses_valid(6) && cross_addresses_valid(10)
  )

  io.done        := false.B
  io.writeEnable := false.B

  val addressWrite = Mux(
    state === State.writeCorners,
    corner_addresses(write_idx),
    cross_addresses(write_idx)
  )

  val addressRead = cross_addresses(read_idx)

  io.address := Mux(io.writeEnable, addressWrite + 400.U, addressRead)

  io.dataWrite := Mux(
    state === State.setBorder || state === State.writeCorners,
    Color.black,
    cross_buffer(write_idx)
  )

  when(io.writeEnable && addressWrite === corner_addresses(0)) {
    cornerWrote(0) := true.B
  }
  when(io.writeEnable && addressWrite === corner_addresses(1)) {
    cornerWrote(1) := true.B
  }
  when(io.writeEnable && addressWrite === corner_addresses(2)) {
    cornerWrote(2) := true.B
  }
  when(io.writeEnable && addressWrite === corner_addresses(3)) {
    cornerWrote(3) := true.B
  }

  switch(state) {

    is(State.idle) {
      when(io.start) {
        state := State.readCenter

        cross_center_x := image_size - 4.U
        cross_center_y := 0.U
        initial_x      := image_size - 4.U
        initial_y      := 0.U

        buffer_idx         := 6.U
        initial_buffer_idx := 6.U

        actual_line  := 0.U
        actual_cross := 0.U

        read_idx  := 0.U
        write_idx := 0.U
      }
    }

    is(State.readCenter) {
      // BLACK CENTER
      when(data_read === Color.black) {
        cross_buffer := VecInit(Seq.fill(5)(Color.black))
        state        := State.writeCross

        bot_diag_buffers(actual_line)(buffer_idx)(0) := Color.unknown
        bot_diag_buffers(actual_line)(buffer_idx)(1) := Color.unknown
        previous_cross_buffers(actual_cross)(0)      := Color.unknown
        previous_cross_buffers(actual_cross)(1)      := Color.unknown

        // WHITE CENTER : GO READ INNER CROSS
      }.otherwise {
        // CENTER CAN BE WHITE ONLY IF ALL INNER PXL EXIST
        cross_buffer(0) := Mux(valid_as_center(0), data_read ,  Color.black)
        state           := State.readInnerCross
        // IF BOT VALID : FIRST BOT
        when(cross_addresses_valid(1) && (valid_as_center(0) || valid_as_center(1))) {
          read_idx := 1.U
        // ELSE GO TOP
        }.elsewhen(cross_addresses_valid(2) && (valid_as_center(0) || valid_as_center(2))) {
          read_idx := 2.U
        }.elsewhen(cross_addresses_valid(3) && (valid_as_center(0) || valid_as_center(3))){
          read_idx := 3.U
        }.elsewhen(cross_addresses_valid(4) && (valid_as_center(0) || valid_as_center(4))){
          read_idx := 4.U
        }.otherwise {
          cross_buffer := VecInit(Seq.fill(5)(Color.black))
          state := State.writeCross
        }
      }
    }

    is(State.writeCross) {
      // WRITING PROCESS
      io.writeEnable := true.B

      // EXIT STATE
      when(write_idx === 4.U && (cross_center_x === 1.U || cross_center_x === image_size - 2.U || cross_center_y === 1.U || cross_center_y === image_size - 2.U)) {
        state := State.setBorder
        when(cross_center_y === image_size - 2.U) {
          write_idx := 5.U
        }.elsewhen(cross_center_x === 1.U) {
          write_idx := 6.U
        }.elsewhen(cross_center_y === 1.U) {
          write_idx := 7.U
        }.otherwise {
          write_idx := 8.U
        }

      }.elsewhen(write_idx === 4.U || (write_idx === 3.U && !cross_addresses_valid(write_idx + 1.U))) {

        state     := State.readCenter
        write_idx := 0.U

        when(cross_center_x < image_size - 2.U && cross_center_y < image_size - 1.U) {
          // CONTINUE DIAGONAL

          cross_center_x := cross_center_x + 2.U
          cross_center_y := cross_center_y + 1.U
          buffer_idx     := buffer_idx + 1.U
          actual_cross   := !actual_cross

        }.otherwise {
          // NEW DIAGONAL

          // RESET NEXT BUFFER AND PREVIOUS
          next_cross_buffers     := VecInit(Seq.fill(2)(Color.unknown))
          previous_cross_buffers := VecInit(
            Seq.fill(2)(VecInit(Seq.fill(2)(Color.unknown)))
          )

          actual_cross := 0.U

          // UPDATE BOT LINE BUFFERS
          bot_diag_buffers(previous_line) := VecInit(
            Seq.fill(10)(VecInit(Seq.fill(2)(Color.unknown)))
          )

          actual_line := previous_line

          // BIGGER DIAGONAL
          when(initial_x >= 5.U) {
            initial_x          := initial_x - 5.U
            cross_center_x     := initial_x - 5.U
            cross_center_y     := initial_y
            buffer_idx         := initial_buffer_idx - 2.U
            initial_buffer_idx := initial_buffer_idx - 2.U
            // SAME SIZE
          }.elsewhen(initial_x >= 1.U && initial_y + 2.U < image_size) {
            initial_x      := initial_x - 1.U
            initial_y      := initial_y + 2.U
            cross_center_x := initial_x - 1.U
            cross_center_y := initial_y + 2.U
            buffer_idx     := initial_buffer_idx
            // SMALLER DIAG
          }.elsewhen(initial_y + 3.U < image_size) {
            initial_x          := initial_x + 1.U
            initial_y          := initial_y + 3.U
            cross_center_x     := initial_x + 1.U
            cross_center_y     := initial_y + 3.U
            buffer_idx         := initial_buffer_idx + 1.U
            initial_buffer_idx := initial_buffer_idx + 1.U
          }.otherwise {
            when(allCornerWrote) {
              state := State.done
            }.otherwise {
              state := State.writeCorners
              when(!cornerWrote(0)) {
                write_idx := 0.U
              }.elsewhen(!cornerWrote(1)) {
                write_idx := 1.U
              }.elsewhen(!cornerWrote(2)) {
                write_idx := 2.U
              }.otherwise {
                write_idx := 3.U
              }
            }
          }
        }

        // UPDATE WRITING IDX FOR NEXT CYCLE
      }.elsewhen(cross_addresses_valid(write_idx + 1.U)) {
        write_idx := write_idx + 1.U
      }.elsewhen(cross_addresses_valid(write_idx + 2.U)) {
        write_idx := write_idx + 2.U
      }.otherwise {
        write_idx := write_idx + 3.U
      }
    }

    is(State.readInnerCross) {

      // ------------------------------
      // Helpers
      // ------------------------------

      def isKnown(x: UInt) = x =/= Color.unknown

      def setCross(idx: UInt, value: UInt) = {
        cross_buffer(idx) := value
        when(value === Color.black) {
          cross_buffer(0) := Color.black
        }
      }

      val exitIdx   = WireDefault(0.U)
      val exitState = WireDefault(0.U)

      when(outer_useful(0) && cross_addresses_valid(5)) {
        exitState := State.readOuterCross
        exitIdx   := 5.U
      }.elsewhen(outer_useful(1) && cross_addresses_valid(6)) {
        exitState := State.readOuterCross
        exitIdx   := 6.U
      }.elsewhen(outer_useful(2) && cross_addresses_valid(7)) {
        exitState := State.readOuterCross
        exitIdx   := 7.U
      }.elsewhen(outer_useful(3) && cross_addresses_valid(8)) {
        exitState := State.readOuterCross
        exitIdx   := 8.U
      }.otherwise {
        exitState := State.writeCross
        exitIdx   := 0.U
      }

      val top_top   = top_actual_diag_buffer(buffer_idx)(0)
      val top_right = top_actual_diag_buffer(buffer_idx)(1)
      val next_top  = next_cross_buffers(0)
      val next_left = next_cross_buffers(1)

      val cross_known = VecInit(
        Seq(
          false.B,
          false.B,
          isKnown(top_top) || isKnown(next_top),
          isKnown(top_right),
          isKnown(next_left)
        )
      )

      // ------------------------------
      // State & Idx transition & READ
      // ------------------------------

      setCross(read_idx, data_read)

      when((cross_addresses_valid(read_idx + 1.U) && !cross_known(read_idx + 1.U)) && (valid_as_center(read_idx + 1.U) || valid_as_center(0)) && read_idx < 4.U) {
        read_idx := read_idx + 1.U
      }.elsewhen((cross_addresses_valid(read_idx + 2.U) && !cross_known(read_idx + 2.U)) && (valid_as_center(read_idx + 2.U) || valid_as_center(0)) && read_idx < 3.U) {
        read_idx := read_idx + 2.U
      }.elsewhen((cross_addresses_valid(read_idx + 3.U) && !cross_known(read_idx + 3.U)) && (valid_as_center(read_idx + 3.U) || valid_as_center(0)) && read_idx < 2.U) {
        read_idx := read_idx + 3.U
      }.otherwise {
        state    := exitState
        read_idx := exitIdx

        // RESET BUFFER VALUES
        next_cross_buffers                 := VecInit(Seq.fill(2)(Color.unknown))
        top_actual_diag_buffer(buffer_idx) := VecInit(Seq.fill(2)(Color.unknown))
      }

      // ------------------------------
      // Buffer writes by read_idx
      // ------------------------------

      switch(read_idx) {
        is(1.U) {
          bot_diag_buffers(actual_line)(buffer_idx)(1) := data_read
          previous_cross_buffers(actual_cross)(1)      := data_read
        }
        is(3.U) {
          previous_cross_buffers(actual_cross)(0) := data_read
        }
        is(4.U) {
          bot_diag_buffers(actual_line)(buffer_idx)(0) := data_read
        }
      }

      // ------------------------------
      // Buffer read & black assignment
      // ------------------------------

      when(!valid_as_center(1.U)){
        cross_buffer(1.U) := Color.black
      }

      when(!valid_as_center(2.U)){
        cross_buffer(2) := Color.black
      }.elsewhen(cross_known(2.U)) {
        when(isKnown(top_top)) {
          setCross(2.U, top_top)
        }.otherwise {
          setCross(2.U, next_top)
        }
      }
      when(!valid_as_center(3.U)){
        cross_buffer(3) := Color.black
      }.elsewhen(cross_known(3.U)) {
        setCross(3.U, top_right)
        previous_cross_buffers(actual_cross)(0) := top_right
      }

      when(!valid_as_center(4.U)){
        cross_buffer(4) := Color.black
      }.elsewhen(cross_known(4.U)) {
        setCross(4.U, next_left)
        bot_diag_buffers(actual_line)(buffer_idx)(0) := next_left
      }
    }

    is(State.readOuterCross) {

      // READ BUFFERED VALUES
      when(previous_cross_buffers(previous_cross)(0) === Color.black) {
        cross_buffer(2) := Color.black
        cross_buffer(4) := Color.black
      }
      when(bot_diag_buffers(previous_line)(buffer_idx)(1) === Color.black) {
        cross_buffer(2) := Color.black
        cross_buffer(3) := Color.black
      }
      when(previous_cross_buffers(previous_cross)(1) === Color.black) {
        cross_buffer(4) := Color.black
      }
      when(bot_diag_buffers(previous_line)(buffer_idx)(0) === Color.black) {
        cross_buffer(2) := Color.black
      }
      when(top_actual_diag_buffer(buffer_idx + 1.U)(0) === Color.black) {
        cross_buffer(3) := Color.black
      }

      // PIXEL BLACK : UPDATE CROSS BUFFER AND OTHER BUFFER
      switch(read_idx) {
        is(5.U) {
          when(data_read === Color.black) {
            cross_buffer(1) := Color.black
            cross_buffer(4) := Color.black
          }
          top_actual_diag_buffer(buffer_idx)(0) := data_read
        }
        is(6.U) {
          when(data_read === Color.black) {
            cross_buffer(2) := Color.black
            cross_buffer(4) := Color.black
          }
        }
        is(7.U) {
          when(data_read === Color.black) {
            cross_buffer(2) := Color.black
            cross_buffer(3) := Color.black
          }
        }
        is(8.U) {
          when(data_read === Color.black) {
            cross_buffer(1) := Color.black
            cross_buffer(3) := Color.black
          }
          next_cross_buffers(1) := data_read
        }
        is(9.U) {
          when(data_read === Color.black) {
            cross_buffer(1) := Color.black
          }
          top_actual_diag_buffer(buffer_idx)(1) := data_read
        }
        is(10.U) {
          when(data_read === Color.black) {
            cross_buffer(4) := Color.black
          }
        }
        is(11.U) {
          when(data_read === Color.black) {
            cross_buffer(2) := Color.black
          }
        }
        is(12.U) {
          when(data_read === Color.black) {
            cross_buffer(3) := Color.black
          }
          next_cross_buffers(0) := data_read
        }
      }

      // EXIT
      when(read_idx === 12.U) {
        state    := State.writeCross
        read_idx := 0.U

      }.elsewhen(!cross_addresses_valid(read_idx + 1.U) || !outer_useful(read_idx + 1.U - 5.U) && read_idx < 11.U) {
        when(!cross_addresses_valid(read_idx + 2.U) || !outer_useful(read_idx + 2.U - 5.U) && read_idx < 10.U) {
          when(!cross_addresses_valid(read_idx + 3.U) || !outer_useful(read_idx + 3.U - 5.U) && read_idx < 9.U) {
            when(!cross_addresses_valid(read_idx + 4.U) || !outer_useful(read_idx + 4.U - 5.U) && read_idx < 8.U) {
              state    := State.writeCross
              read_idx := 0.U
            }.otherwise {
              read_idx := read_idx + 4.U
            }
          }.otherwise {
            read_idx := read_idx + 3.U
          }
        }.otherwise {
          read_idx := read_idx + 2.U
        }
      }.otherwise {
        read_idx := read_idx + 1.U
      }
    }

    is(State.setBorder) {
      io.writeEnable := true.B

      when(cross_center_x === 1.U && write_idx < 6.U) {
        write_idx := 6.U
      }.elsewhen(cross_center_y === 1.U && write_idx < 7.U) {
        write_idx := 7.U
      }.elsewhen(cross_center_x === image_size - 2.U && write_idx < 8.U) {
        write_idx := 8.U
      }.otherwise {
        state     := State.readCenter
        write_idx := 0.U
        when(cross_center_x < image_size - 2.U && cross_center_y < image_size - 1.U) {
          // CONTINUE DIAGONAL
          cross_center_x := cross_center_x + 2.U
          cross_center_y := cross_center_y + 1.U
          buffer_idx     := buffer_idx + 1.U
          actual_cross   := !actual_cross

        }.otherwise {
          // NEW DIAGONAL

          // RESET NEXT BUFFER AND PREVIOUS
          next_cross_buffers     := VecInit(Seq.fill(2)(Color.unknown))
          previous_cross_buffers := VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(Color.unknown))))
          actual_cross           := 0.U

          // UPDATE BOT LINE BUFFERS
          bot_diag_buffers(previous_line) := VecInit(Seq.fill(10)(VecInit(Seq.fill(2)(Color.unknown))))
          actual_line                     := previous_line

          when(initial_x >= 5.U) {
            // BIGGER DIAGONAL
            initial_x          := initial_x - 5.U
            cross_center_x     := initial_x - 5.U
            cross_center_y     := initial_y
            buffer_idx         := initial_buffer_idx - 2.U
            initial_buffer_idx := initial_buffer_idx - 2.U
          }.elsewhen(initial_x >= 1.U && initial_y + 2.U < image_size) {
            // SAME SIZE
            initial_x      := initial_x - 1.U
            initial_y      := initial_y + 2.U
            cross_center_x := initial_x - 1.U
            cross_center_y := initial_y + 2.U
            buffer_idx     := initial_buffer_idx
          }.elsewhen(initial_y + 3.U < image_size) {
            // SMALLER DIAG
            initial_x          := initial_x + 1.U
            initial_y          := initial_y + 3.U
            cross_center_x     := initial_x + 1.U
            cross_center_y     := initial_y + 3.U
            buffer_idx         := initial_buffer_idx + 1.U
            initial_buffer_idx := initial_buffer_idx + 1.U
          }.otherwise {
            when(allCornerWrote) {
              state := State.done
            }.otherwise {
              state := State.writeCorners
              when(!cornerWrote(0)) {
                write_idx := 0.U
              }.elsewhen(!cornerWrote(1)) {
                write_idx := 1.U
              }.elsewhen(!cornerWrote(2)) {
                write_idx := 2.U
              }.otherwise {
                write_idx := 3.U
              }
            }
          }
        }
      }
    }

    is(State.done) {
      io.done := true.B
    }

    is(State.writeCorners) {
      io.writeEnable := true.B

      when(!cornerWrote(1) && write_idx =/= 1.U) {
        write_idx := 1.U
      }.elsewhen(!cornerWrote(2) && write_idx =/= 2.U) {
        write_idx := 2.U
      }.elsewhen(!cornerWrote(3) && write_idx =/= 3.U) {
        write_idx := 3.U
      }.otherwise {
        state := State.done
      }
    }
  }
}
