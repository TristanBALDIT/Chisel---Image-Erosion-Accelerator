import chisel3._
import chisel3.util._

class AcceleratorOpti extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  val buffer = RegInit(VecInit(Seq.fill(3)(VecInit(Seq.fill(20)(0.U(8.W))))))

  val idle :: initialRead :: firstLineWrite :: computeWrite :: readLine :: readAbove :: lastLineWrite :: done :: Nil = Enum(8)

  val line_top = RegInit(0.U(3.W))
  val pxl_idx = RegInit(0.U(5.W))

  val lineRead_cnt = RegInit(0.U(5.W))
  val state = RegInit(idle)

  val addressRead = RegInit(0.U(16.W))
  val addressWrite = RegInit(0.U(16.W))

  val line_mid = (line_top + 1.U) % 3.U
  val line_bottom = (line_top + 2.U) % 3.U

  val leftIdx = Mux(pxl_idx === 0.U, 0.U, pxl_idx - 1.U)
  val rightIdx = Mux(pxl_idx === 19.U, 19.U, pxl_idx + 1.U)

  val readJump = RegInit(false.B)

  val whiteCheck =
    (buffer(line_top)(pxl_idx) === 255.U) &&
      (buffer(line_mid)(leftIdx) === 255.U) &&
      (buffer(line_mid)(pxl_idx) === 255.U) &&
      (buffer(line_mid)(rightIdx) === 255.U) &&
      (buffer(line_bottom)(pxl_idx) === 255.U)

  io.dataWrite := Mux(whiteCheck, 255.U, 0.U)

  io.done := false.B
  io.writeEnable := false.B
  io.address := Mux(io.writeEnable, addressWrite, addressRead)

  switch(state) {

    is(idle) {
      when(io.start) {
        state := initialRead
        addressRead := 0.U
        addressWrite := 400.U
        line_top := 0.U
        pxl_idx := 0.U
        lineRead_cnt := 0.U
      }
    }

    is(initialRead) {
      buffer(lineRead_cnt)(pxl_idx) := io.dataRead(7,0)
      addressRead := addressRead + 1.U

      when(lineRead_cnt === 2.U){
        when(pxl_idx === 19.U){
          state := firstLineWrite
          pxl_idx := 0.U
          lineRead_cnt := lineRead_cnt + 1.U
        }.otherwise{
          pxl_idx := pxl_idx + 1.U
        }
      }.otherwise{
        when(pxl_idx === 19.U){
          pxl_idx := 0.U
          lineRead_cnt := lineRead_cnt + 1.U
        }.otherwise{
          pxl_idx := pxl_idx + 1.U
        }
      }
    }

    is(firstLineWrite){
      io.writeEnable := true.B
      addressWrite := addressWrite + 1.U
      io.dataWrite := 0.U
      when(addressWrite === 419.U){
        state := computeWrite
      }
    }

    is(computeWrite){
      io.writeEnable := true.B
      addressWrite := addressWrite + 1.U
      when(pxl_idx === 19.U){
        when(lineRead_cnt === 20.U){
          state := lastLineWrite
        }.otherwise{
          pxl_idx := 0.U
          state := readLine
        }
      }.otherwise{
        pxl_idx := pxl_idx + 1.U
      }
    }

    is(readLine){
      when(pxl_idx === 19.U){
        buffer(line_top)(pxl_idx) := io.dataRead(7,0)
        pxl_idx := 0.U
        line_top := (line_top + 1.U) % 3.U
        state := computeWrite
        addressRead := addressRead + 1.U
        lineRead_cnt := lineRead_cnt + 1.U
      }.otherwise{
        buffer(line_top)(pxl_idx) := io.dataRead(7,0)

        val topRight = buffer(line_bottom)(pxl_idx + 1.U)

        when(readJump){                                           // force jump after going back
          addressRead := addressRead + 2.U
          pxl_idx := pxl_idx + 2.U
          readJump := false.B
        }.otherwise{
          when(io.dataRead(7,0) === 0.U && topRight =/= 255.U){    // skip and mark next pixel
            buffer(line_top)(pxl_idx+1.U) := 1.U
            when(pxl_idx === 18.U){
              pxl_idx := 0.U
              line_top := (line_top + 1.U) % 3.U
              state := computeWrite
              addressRead := addressRead + 2.U
              lineRead_cnt := lineRead_cnt + 1.U
            }.otherwise {
              addressRead := addressRead + 2.U
              pxl_idx := pxl_idx + 2.U
            }
          }.otherwise{
            when(io.dataRead(7,0) === 255.U){                     // if white and ...
              switch(buffer(line_bottom)(pxl_idx)){               // ... top unknown : read top
                is(1.U){
                  state := readAbove
                  addressRead := addressRead - 20.U
                }
                is(0.U){                                          // ... top B : go next pxl
                  addressRead := addressRead + 1.U
                  pxl_idx := pxl_idx + 1.U
                }
                is(255.U){                                        // ... top W :
                  when(buffer(line_top)(pxl_idx-1.U) === 1.U){    //    -  if left unknow go check
                    addressRead := addressRead - 1.U
                    pxl_idx := pxl_idx - 1.U
                    readJump := true.B
                  }.otherwise{                                   //    -  otherwise go right
                    addressRead := addressRead + 1.U
                    pxl_idx := pxl_idx + 1.U
                  }
                }
              }
            }.otherwise{
              addressRead := addressRead + 1.U
              pxl_idx := pxl_idx + 1.U
            }
          }
        }
      }
    }

    is(readAbove){
      buffer(line_bottom)(pxl_idx) := io.dataRead(7,0)
      state := readLine
      when(io.dataRead(7,0) === 255.U && buffer(line_top)(pxl_idx-1.U) === 1.U){
        addressRead := addressRead + 19.U
        pxl_idx := pxl_idx - 1.U
        readJump := true.B
      }.otherwise{
        addressRead := addressRead + 21.U
        pxl_idx := pxl_idx + 1.U
      }
    }

    is(lastLineWrite){
      io.writeEnable := true.B
      addressWrite := addressWrite + 1.U
      io.dataWrite := 0.U
      when(addressWrite === 799.U){
        state := done
      }
    }

    is(done) {
      io.done := true.B
      state := done
    }
  }
}