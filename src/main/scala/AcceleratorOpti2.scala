import chisel3._
import chisel3.util._

class AcceleratorOpti2 extends Module {
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

  val centerRead :: leftReadB :: leftReadW :: topRead  :: rightRead ::  topLeftRead :: topRightRead :: extraLeftRead :: Nil = Enum(7)

  val line_top = RegInit(0.U(3.W))
  val pxl_idx = RegInit(0.U(5.W))

  val lineRead_cnt = RegInit(0.U(5.W))
  val state = RegInit(idle)
  val readState = RegInit(centerRead)

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


      when(pxl_idx >= 19.U) {
        pxl_idx := 0.U
        line_top := (line_top + 1.U) % 3.U
        state := computeWrite
        addressRead := addressRead + 1.U
        lineRead_cnt := lineRead_cnt + 1.U

        when(pxl_idx === 19.U){
          buffer(line_top)(pxl_idx) := io.dataRead(7,0)
        }

      //CENTER READ
      }.elsewhen(readState === centerRead) {

        buffer(line_top)(pxl_idx) := io.dataRead(7,0)
        buffer(line_top)(pxl_idx-1.U) := 1.U
        buffer(line_top)(pxl_idx+1.U) := 1.U

        // PXL BLACK
        when(io.dataRead(7,0) === 0.U){

          // TOP LEFT WHITE
          when(buffer(line_bottom)(pxl_idx-1.U) ===  255.U){

            readState := leftReadB
            addressRead := addressRead - 1.U
            pxl_idx := pxl_idx - 1.U

          // ELSE IF TO RIGHT WHITE
          }.elsewhen(buffer(line_bottom)(pxl_idx+1.U) ===  255.U){

            readState := rightRead
            addressRead := addressRead + 1.U
            pxl_idx := pxl_idx + 1.U

          // OTHERWISE LEFT AND RIGHT STAY MARKED X
          }.otherwise{
            addressRead := addressRead + 3.U
            pxl_idx := pxl_idx + 3.U
          }

        // PXL WHITE
        }.otherwise{
          switch(buffer(line_bottom)(pxl_idx)){

            // TOP X -> CHECK TOP
            is(1.U){
              readState := topRead
              addressRead := addressRead - 20.U
            }

            // TOP WHITE -> NEIGHBOR LOGIC
            is(0.U){
              // TOP LEFT WHITE
              when(buffer(line_bottom)(pxl_idx-1.U) ===  255.U){
                readState := leftRead
                addressRead := addressRead - 1.U
                pxl_idx := pxl_idx - 1.U

                // ELSE IF TOP RIGHT WHITE
              }.elsewhen(buffer(line_bottom)(pxl_idx+1.U) ===  255.U){
                buffer(line_bottom)(pxl_idx-1.U) := 1.U
                readState := rightRead
                addressRead := addressRead + 1.U
                pxl_idx := pxl_idx + 1.U

                // OTHERWISE LEFT AND RIGHT MARKED X
              }.otherwise{
                buffer(line_bottom)(pxl_idx-1.U) := 1.U
                buffer(line_bottom)(pxl_idx+1.U) := 1.U
                addressRead := addressRead + 3.U
                pxl_idx := pxl_idx + 3.U
              }
            }

            // TOP WHITE -> CHECK LEFT
            is(255.U){
              readState := leftReadW
              addressRead := addressRead - 1.U
              pxl_idx := pxl_idx - 1.U
            }
          }
        }

      // TOP CHECK (CENTER CELL WHITE)
      }.elsewhen(readState === topRead) {

        buffer(line_bottom)(pxl_idx) := io.dataRead(7, 0)

        // TOP BLACK
        when(io.dataRead(7, 0) === 0.U) {
          // TODO SET CENTER AS 254
          switch(buffer(line_bottom)(pxl_idx - 1.U)) {

            // TOP LEFT WHITE
            is(255.U) {
              readState := leftRead
              addressRead := addressRead - 19.U
              pxl_idx := pxl_idx - 1.U
            }

            // TOP LEFT BLACK
            is(0.U) {

              switch(buffer(line_bottom)(pxl_idx + 1.U)) {

                // TOP RIGHT WHITE
                is(255.U) {
                  readState := rightRead
                  addressRead := addressRead + 21.U
                  pxl_idx := pxl_idx + 1.U
                }

                // TOP RIGHT BLACK
                is(0.U) {
                  readState := centerRead
                  addressRead := addressRead + 23.U
                  pxl_idx := pxl_idx + 3.U
                }

                // TOP RIGHT UNKNOWN
                is(1.U){
                  readState := topRightRead
                  addressRead := addressRead + 1.U
                  pxl_idx := pxl_idx + 1.U
                }
              }
            }

            // TODO DUPLICATE TOP LEFT BLACK LOGIC FOR 254 CASE

            // TOP LEFT 1
            is(1.U) {
              readState := topLeftRead
              addressRead := addressRead - 1.U
              pxl_idx := pxl_idx - 1.U
            }
          }

        // TOP WHITE -> READ LEFT
        }.otherwise{
          readState := leftRead
          addressRead := addressRead - 19.U
          pxl_idx := pxl_idx - 1.U
        }

      // LEFT READ
      }.elsewhen(readState === leftRead){

        buffer(line_top)(pxl_idx) := io.dataRead(7, 0)

        when(buffer(line_top)(pxl_idx+1.U) === 255.U && io.dataRead(7, 0) === 0.U){
          // TODO SET CENTER AS 254
        }


        when(buffer(line_top)(pxl_idx+1.U) === 0.U || io.dataRead(7, 0) === 0.U){

          when(io.dataRead(7, 0) === 255.U){
            // TODO SET LEFT AS 254
          }

          // TOP RIGHT WHITE
          when(buffer(line_bottom)(pxl_idx+2.U) === 255.U){
            readState := rightRead
            addressRead := addressRead + 2.U
            pxl_idx := pxl_idx + 2.U

            // TOP RIGHT 1
          }.elsewhen(buffer(line_bottom)(pxl_idx+2.U) === 1.U){
            readState := topRightRead
            addressRead := addressRead - 18.U
            pxl_idx := pxl_idx + 2.U

            // ELSE (BLACK OR 254)
          }.otherwise {
            buffer(line_top)(pxl_idx+2.U) := 1.U
            readState := centerRead
            addressRead := addressRead + 4.U
            pxl_idx := pxl_idx + 2.U
          }

        // LEFT CAN BE CENTER OF PATTERN,TOP LEFT UNKNOWN : CHECK TOP LEFT FIRST
        }.elsewhen(buffer(line_bottom)(pxl_idx) === 1.U && pxl_idx > 0.U) {

          readState := topLeftRead
          addressRead := addressRead - 20 .U
          pxl_idx := pxl_idx

        // CENTER, LEFT AND TOP LEFT WHITE --> LEFT CAN BE CENTER OF PATTERN : EXTRA LEFT
        }.elsewhen((buffer(line_bottom)(pxl_idx) === 254.U || buffer(line_bottom)(pxl_idx) === 255.U) && pxl_idx > 0.U) {

          // EXTRA LEFT CHECK NEEDED
          when(buffer(line_top)(pxl_idx-1.U) === 1.U){
            readState := extraLeftRead
            addressRead := addressRead - 1.U
            pxl_idx := pxl_idx - 1.U

          // TOP WHITE or 254 -> read right
          }.elsewhen(buffer(line_bottom)(pxl_idx+1.U) =/= 0.U) {
            readState := rightRead
            addressRead := addressRead + 1.U
            pxl_idx := pxl_idx - 1.U

          //OTHERWISE
          }.otherwise{

            // TOP RIGHT WHITE
            when(buffer(line_bottom)(pxl_idx+2.U) === 255.U){
              readState := rightRead
              addressRead := addressRead + 2.U
              pxl_idx := pxl_idx + 2.U

              // TOP RIGHT 1
            }.elsewhen(buffer(line_bottom)(pxl_idx+2.U) === 1.U){
              readState := topRightRead
              addressRead := addressRead - 18.U
              pxl_idx := pxl_idx + 2.U

              // ELSE (BLACK OR 254)
            }.otherwise {
              buffer(line_top)(pxl_idx+2.U) := 1.U
              readState := centerRead
              addressRead := addressRead + 4.U
              pxl_idx := pxl_idx + 2.U
            }
          }
        }
      }.elsewhen(readState === topLeftRead){
        buffer(line_bottom)(pxl_idx) := io.dataRead(7, 0)

        when(io.dataRead(7, 0) === 0.U){

          // LEFT ALREADY KNOWN AS WHITE : CENTER LEFT AN TOP WHITE : GO RIGHT
          when(buffer(line_top)(pxl_idx) === 255.U){
            // TODO CHANGE LEFT TO 254
            readState := rightRead
            addressRead := addressRead + 22.U
            pxl_idx := pxl_idx + 2.U

          // CHECK IF RIGHT NEEDED
          }.otherwise {
            switch(buffer(line_bottom)(pxl_idx + 1.U)) {

              // TOP RIGHT WHITE
              is(255.U) {
                readState := rightRead
                addressRead := addressRead + 22.U
                pxl_idx := pxl_idx + 2.U
              }

              // TOP RIGHT BLACK
              is(0.U) {
                readState := centerRead
                addressRead := addressRead + 24.U
                pxl_idx := pxl_idx + 4.U
              }

              // TOP RIGHT UNKNOWN
              is(1.U){
                readState := topRightRead
                addressRead := addressRead + 2.U
                pxl_idx := pxl_idx + 2.U
              }
            }
          }
        // TOP LEFT WHITE
        }.otherwise {

          // LEFT ALREADY KNOWN AS WHITE : GO EXTRA LEFT
          when(buffer(line_top)(pxl_idx) === 255.U && pxl_idx > 0.U){
            readState := extraLeftRead
            addressRead := addressRead + 19.U
            pxl_idx := pxl_idx - 1.U

          // CHECK LEFT NEEDED
          }.otherwise {
            readState := leftRead
            addressRead := addressRead + 20.U
          }
        }
      // EXTRA LEFT READ
      }.elsewhen(readState === extraLeftRead){
        buffer(line_bottom)(pxl_idx) := io.dataRead(7, 0)

        // CHECK RIGHT LOGIC
        switch(buffer(line_bottom)(pxl_idx + 1.U)) {

          // TOP RIGHT WHITE
          is(255.U) {
            readState := rightRead
            addressRead := addressRead + 3.U
            pxl_idx := pxl_idx + 3.U
          }

          // TOP RIGHT BLACK
          is(0.U) {
            readState := centerRead
            addressRead := addressRead + 5.U
            pxl_idx := pxl_idx + 5.U
          }

          // TOP RIGHT UNKNOWN
          is(1.U){
            readState := topRightRead
            addressRead := addressRead + 23.U
            pxl_idx := pxl_idx + 3.U
          }
        }
      // RIGHT READ
      }.elsewhen(readState === rightRead){

      }
    }













    is(readLine){
      when(pxl_idx > 19.U){
        pxl_idx := 0.U
        line_top := (line_top + 1.U) % 3.U
        state := computeWrite
        addressRead := addressRead + 1.U
        lineRead_cnt := lineRead_cnt + 1.U
      }.otherwise{
        switch(readState){
          is(centerRead){
            buffer(line_top)(pxl_idx) := io.dataRead(7,0)
            when(io.dataRead(7,0) === 0.U){
              when(buffer(line_bottom)(pxl_idx-1.U) ===  255.U){
                readState := leftReadB
                addressRead := addressRead - 1.U
                pxl_idx := pxl_idx - 1.U
              }.otherwise {
                buffer(line_top)(pxl_idx-1.U) := 1.U
                when(buffer(line_bottom)(pxl_idx+1.U) ===  255.U){
                  readState := rightRead
                  addressRead := addressRead + 1.U
                  pxl_idx := pxl_idx + 1.U
                }.otherwise {
                  buffer(line_top)(pxl_idx+1.U) := 1.U
                  addressRead := addressRead + 3.U
                  pxl_idx := pxl_idx + 3.U
                }
              }
            }.otherwise {
              switch(buffer(line_bottom)(pxl_idx)){
                is(1.U){                                            // if top = X , read top first
                  readState := topRead
                  addressRead := addressRead - 20.U
                }
                is(255.U){                                         // if top = W , read left
                  readState := leftReadW
                  addressRead := addressRead - 1.U
                  pxl_idx := pxl_idx - 1.U
                }
                is(0.U){
                  buffer(line_top)(pxl_idx-1.U) := 254.U            // set center pixel as Bx if top = N
                  switch(buffer(line_bottom)(pxl_idx-1.U)){
                    is(0.U){                                        // if topleft N set left as X
                      buffer(line_top)(pxl_idx-1.U) := 1.U
                      switch(buffer(line_bottom)(pxl_idx+1.U)){
                        is(0.U){                                        // if topright N set left as N
                          buffer(line_top)(pxl_idx-1.U) := 1.U
                          addressRead := addressRead + 3.U
                          pxl_idx := pxl_idx + 3.U
                        }
                        is(1.U){                                        // if topright X set go test topright
                          readState := topRightRead
                          addressRead := addressRead - 19.U
                          pxl_idx := pxl_idx + 1.U
                        }
                        is(255.U){                                      // if topright W set go test right
                          readState := rightRead
                          addressRead := addressRead + 1.U
                          pxl_idx := pxl_idx + 1.U
                        }
                      }
                    }
                    is(1.U){                                      // if topleft X set go test topleft
                      readState := topLeftRead
                      addressRead := addressRead - 21.U
                      pxl_idx := pxl_idx - 1.U
                    }
                    is(255.U){                                  // if topleft W set go test left
                      readState := leftReadW
                      addressRead := addressRead - 1.U
                      pxl_idx := pxl_idx - 1.U
                    }
                  }
                }
              }
            }
          }
          is(topRead){                                        //TEST TOP
            buffer(line_bottom)(pxl_idx) := io.dataRead(7,0)
            when(io.dataRead(7,0) === 0.U){
              buffer(line_top)(pxl_idx) := 254.U              //if top N set center as Bx
              switch(buffer(line_bottom)(pxl_idx-1.U)) {
                is(0.U) {                                     // if topleft N set left as X
                  buffer(line_top)(pxl_idx - 1.U) := 1.U
                  switch(buffer(line_bottom)(pxl_idx + 1.U)) {      // if topright N set left as N
                    is(0.U) {
                      buffer(line_top)(pxl_idx - 1.U) := 1.U
                      addressRead := addressRead + 23.U
                      pxl_idx := pxl_idx + 3.U
                    }
                    is(1.U) {                                       // if topright X set go test topright
                      readState := topRightRead
                      addressRead := addressRead + 1.U
                      pxl_idx := pxl_idx + 1.U
                    }
                    is(255.U) {                                     // if topright W set go test right
                      readState := rightRead
                      addressRead := addressRead + 21.U
                      pxl_idx := pxl_idx + 1.U
                    }
                  }
                }
                is(1.U) {                                          // if topleft X set go test topleft
                  readState := topLeftRead
                  addressRead := addressRead - 1.U
                  pxl_idx := pxl_idx - 1.U
                }
                is(255.U) {                                       // if topleft W set go test left
                  readState := leftReadW
                  addressRead := addressRead - 19.U
                  pxl_idx := pxl_idx - 1.U
                }
              }
            }
          }
          is(leftReadB){
            when(io.dataRead(7,0) === 255.U){                         // read and set as Bx or N
              buffer(line_top)(pxl_idx) := io.dataRead(7,0) - 1.U
            }.otherwise {
              buffer(line_top)(pxl_idx) := io.dataRead(7,0)
            }

            when(buffer(line_bottom)(pxl_idx+2.U) ===  255.U){        // if topright is W read right
              readState := rightRead
              addressRead := addressRead + 2.U
              pxl_idx := pxl_idx + 2.U
            }.otherwise {                                             // else set it a X
              buffer(line_top)(pxl_idx+2.U) := 1.U
              addressRead := addressRead + 4.U
              pxl_idx := pxl_idx + 4.U
            }
          }
          is(leftReadW){
            when(io.dataRead(7,0) === 0.U){
              buffer(line_top)(pxl_idx+1.U) ===  254.U
            }
            buffer(line_top)(pxl_idx) := io.dataRead(7,0)
            when((buffer(line_bottom)(pxl_idx) ===  255.U || buffer(line_bottom)(pxl_idx) ===  254.U) && io.dataRead(7,0) === 255.U){
              readState := extraLeftRead
              pxl_idx := pxl_idx - 1.U
              addressRead := addressRead - 1.U
            }.otherwise{
              when(buffer(line_bottom)(pxl_idx+2.U) ===  255.U || buffer(line_bottom)(pxl_idx+2.U) ===  254.U || io.dataRead(7,0) === 255.U) {
                readState := rightRead
                addressRead := addressRead + 2.U
                pxl_idx := pxl_idx + 2.U
              }.otherwise{
                when(buffer(line_bottom)(pxl_idx+2.U) ===  1.U){
                  readState := topRightRead
                  addressRead := addressRead - 19.U
                  pxl_idx := pxl_idx + 1.U
                }.otherwise {
                  readState := centerRead
                  buffer(line_top)(pxl_idx+2.U) := 1.U
                  addressRead := addressRead + 4.U
                  pxl_idx := pxl_idx + 4.U
                }
              }
            }
          }
          is(topLeftRead){
            buffer(line_bottom)(pxl_idx) := io.dataRead(7,0)
            switch(io.dataRead(7,0)){
              is(0.U){
                buffer(line_top)(pxl_idx) := 1.U
                when(buffer(line_bottom)(pxl_idx+2.U) ===  255.U || buffer(line_bottom)(pxl_idx+2.U) ===  254.U || io.dataRead(7,0) === 255.U) {
                  readState := rightRead
                  addressRead := addressRead + 2.U
                  pxl_idx := pxl_idx + 2.U
                }.otherwise{
                  when(buffer(line_bottom)(pxl_idx+2.U) ===  1.U){
                    readState := topRightRead
                    addressRead := addressRead - 19.U
                    pxl_idx := pxl_idx + 1.U
                  }.otherwise {
                    readState := centerRead
                    buffer(line_top)(pxl_idx+2.U) := 1.U
                    addressRead := addressRead + 4.U
                    pxl_idx := pxl_idx + 4.U
                  }
                }
              }
              is(255.U){
                readState := leftReadW
                addressRead := addressRead + 20.U
              }
            }
          }
          is(topRightRead){
            buffer(line_bottom)(pxl_idx) := io.dataRead(7,0)
            switch(io.dataRead(7,0)){
              is(0.U){
                buffer(line_top)(pxl_idx) := 1.U
                readState := centerRead
              }
              is(255.U){
                readState := rightRead
                addressRead := addressRead + 20.U
              }
            }
          }
          is(rightRead){
            buffer(line_top)(pxl_idx) := io.dataRead(7,0)
            readState := centerRead
          }
        }
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