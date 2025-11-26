import chisel3._
import chisel3.util._

class AcceleratorOpti extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done  = Output(Bool())

    val address     = Output(UInt(16.W))
    val dataRead    = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite   = Output(UInt(32.W))

  })

  val buffer = RegInit(VecInit(Seq.fill(3)(VecInit(Seq.fill(20)(0.U(8.W))))))

  val idle :: initialRead :: firstLineWrite :: computeWrite :: readLine :: readAbove :: lastLineWrite :: done :: Nil =
    Enum(8)

  val line_top = RegInit(0.U(2.W))
  val pxl_idx  = RegInit(0.U(5.W))

  val lineRead_cnt = RegInit(0.U(5.W))
  val state        = RegInit(idle)

  val addressRead  = RegInit(0.U(16.W))
  val addressWrite = RegInit(0.U(16.W))

  val line_mid    = Wire(UInt(2.W))
  val line_bottom = Wire(UInt(2.W))
  line_mid    := 0.U
  line_bottom := 0.U

  switch(line_top) {
    is(0.U) {
      line_mid    := 1.U
      line_bottom := 2.U
    }
    is(1.U) {
      line_mid    := 2.U
      line_bottom := 0.U
    }
    is(2.U) {
      line_mid    := 0.U
      line_bottom := 1.U
    }
  }

  val leftIdx  = Mux(pxl_idx === 0.U, 0.U, pxl_idx - 1.U)
  val rightIdx = Mux(pxl_idx === 19.U, 19.U, pxl_idx + 1.U)

  val readJump = RegInit(false.B)

  val whiteCheck =
    (buffer(line_top)(pxl_idx) === 255.U) &&
      (buffer(line_mid)(leftIdx) === 255.U) &&
      (buffer(line_mid)(pxl_idx) === 255.U) &&
      (buffer(line_mid)(rightIdx) === 255.U) &&
      (buffer(line_bottom)(pxl_idx) === 255.U)

  io.dataWrite := Mux(whiteCheck, 255.U, 0.U)

  io.done        := false.B
  io.writeEnable := false.B
  io.address     := Mux(io.writeEnable, addressWrite, addressRead)

  switch(state) {

    is(idle) {
      when(io.start) {
        state        := initialRead
        addressRead  := 0.U
        addressWrite := 400.U
        line_top     := 0.U
        pxl_idx      := 0.U
        lineRead_cnt := 0.U
      }
    }

    is(initialRead) {
      val isLastPixel       = pxl_idx === 19.U
      val isBeforeLastPixel = pxl_idx === 18.U

      // DATA STORED
      buffer(line_top)(pxl_idx) := io.dataRead(7, 0)

      // 3 LINES READ
      when((isLastPixel || isBeforeLastPixel) && lineRead_cnt === 2.U) {
        state := firstLineWrite
      }

      // END OF LINE
      when(isLastPixel) {

        pxl_idx      := 0.U
        line_top     := Mux(line_top === 2.U, 0.U, line_top + 1.U)
        addressRead  := addressRead + 1.U
        lineRead_cnt := lineRead_cnt + 1.U

        // BASE CASE
      }.otherwise {

        // TOP RIGHT PIXEL VALUE
        val topRight =
          Mux(lineRead_cnt === 0.U, 0.U, buffer(line_bottom)(pxl_idx + 1.U))

        // FORCED JUMP FOR EFFICIENCY
        when(readJump) {

          addressRead := addressRead + 2.U
          pxl_idx     := pxl_idx + 2.U
          readJump    := false.B

          // PIXEL BLACK AND TOP RIGHT WHITE
        }.elsewhen(io.dataRead(7, 0) === 0.U && topRight =/= 255.U) {

          buffer(line_top)(pxl_idx + 1.U) := 1.U
          when(isBeforeLastPixel) {
            pxl_idx      := 0.U
            line_top     := Mux(line_top === 2.U, 0.U, line_top + 1.U)
            addressRead  := addressRead + 2.U
            lineRead_cnt := lineRead_cnt + 1.U
          }.otherwise {
            addressRead := addressRead + 2.U
            pxl_idx     := pxl_idx + 2.U
          }

          // PIXEL WHITE
        }.elsewhen(io.dataRead(7, 0) === 255.U) {

          // VALUES OF TOP CHOOSE NEXT READ : TOP,LEFT OR RIGHT
          switch(buffer(line_bottom)(pxl_idx)) {
            is(1.U) {
              state       := readAbove
              addressRead := addressRead - 20.U
            }
            is(0.U) { // ... top B : go next pxl
              addressRead := addressRead + 1.U
              pxl_idx     := pxl_idx + 1.U
            }
            is(255.U) { // ... top W :
              when(buffer(line_top)(pxl_idx - 1.U) === 1.U) { //    -  if left unknow go check
                addressRead := addressRead - 1.U
                pxl_idx     := pxl_idx - 1.U
                readJump    := true.B
              }.otherwise { //    -  otherwise go right
                addressRead := addressRead + 1.U
                pxl_idx     := pxl_idx + 1.U
              }
            }
          }

          // PIXEL BLACK AND TOP RIGHT WHITE
        }.otherwise {
          addressRead := addressRead + 1.U
          pxl_idx     := pxl_idx + 1.U
        }
      }
    }

    is(firstLineWrite) {
      io.writeEnable := true.B
      addressWrite   := addressWrite + 1.U
      io.dataWrite   := 0.U
      when(addressWrite === 419.U) {
        state := computeWrite
      }
    }

    is(computeWrite) {
      io.writeEnable := true.B
      addressWrite   := addressWrite + 1.U
      when(pxl_idx === 19.U) {
        when(lineRead_cnt === 20.U) {
          state := lastLineWrite
        }.otherwise {
          pxl_idx := 0.U
          state   := readLine
        }
      }.otherwise {
        pxl_idx := pxl_idx + 1.U
      }
    }

    is(readLine) {

      buffer(line_top)(pxl_idx) := io.dataRead(7, 0)

      val topRight = buffer(line_bottom)(pxl_idx + 1.U)

      // END OF LINE
      when(pxl_idx === 19.U) {

        pxl_idx      := 0.U
        line_top     := Mux(line_top === 2.U, 0.U, line_top + 1.U)
        state        := computeWrite
        addressRead  := addressRead + 1.U
        lineRead_cnt := lineRead_cnt + 1.U

        // FORCED FORWARD JUMP FOR EFFICIENCY
      }.elsewhen(readJump) {

        addressRead := addressRead + 2.U
        pxl_idx     := pxl_idx + 2.U
        readJump    := false.B

        // PXL BLACK AND TOP RIGHT NOT WHITE
      }.elsewhen(io.dataRead(7, 0) === 0.U && topRight =/= 255.U) {

        buffer(line_top)(pxl_idx + 1.U) := 1.U
        when(pxl_idx === 18.U) {
          pxl_idx      := 0.U
          line_top     := Mux(line_top === 2.U, 0.U, line_top + 1.U)
          state        := computeWrite
          addressRead  := addressRead + 2.U
          lineRead_cnt := lineRead_cnt + 1.U
        }.otherwise {
          addressRead := addressRead + 2.U
          pxl_idx     := pxl_idx + 2.U
        }

        // PXL WHITE
      }.elsewhen(io.dataRead(7, 0) === 255.U) {

        // TOP VALUE DECIDE NEXT CHECK
        switch(buffer(line_bottom)(pxl_idx)) {
          is(1.U) { // TOP UNKNOWN : READ TOP
            state       := readAbove
            addressRead := addressRead - 20.U
          }
          is(0.U) { // TOP BLACK : READ RIGHT
            addressRead := addressRead + 1.U
            pxl_idx     := pxl_idx + 1.U
          }
          is(255.U) { // TOP WHITE :
            when(buffer(line_top)(pxl_idx - 1.U) === 1.U) { //  IF LEFT UNKNOWN GO LEFT
              addressRead := addressRead - 1.U
              pxl_idx     := pxl_idx - 1.U
              readJump    := true.B
            }.otherwise { //  OTHERWISE GO RIGHT
              addressRead := addressRead + 1.U
              pxl_idx     := pxl_idx + 1.U
            }
          }
        }

        // PIXEL BLACK AND TOP RIGHT WHITE
      }.otherwise {
        addressRead := addressRead + 1.U
        pxl_idx     := pxl_idx + 1.U
      }
    }

    is(readAbove) {
      buffer(line_bottom)(pxl_idx) := io.dataRead(7, 0)
      state                        := readLine
      when(
        io.dataRead(7, 0) === 255.U && buffer(line_top)(pxl_idx - 1.U) === 1.U
      ) {
        addressRead := addressRead + 19.U
        pxl_idx     := pxl_idx - 1.U
        readJump    := true.B
      }.otherwise {
        addressRead := addressRead + 21.U
        pxl_idx     := pxl_idx + 1.U
      }
    }

    is(lastLineWrite) {
      io.writeEnable := true.B
      addressWrite   := addressWrite + 1.U
      io.dataWrite   := 0.U
      when(addressWrite === 799.U) {
        state := done
      }
    }

    is(done) {
      io.done := true.B
      state   := done
    }
  }
}
