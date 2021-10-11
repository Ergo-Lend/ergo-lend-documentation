package contracts

object ServiceContract {
  /**
   * ServiceBox
   *
   * 0. Why and what is it for?
   * 1. Figure out the cases where it is used
   * 2. Compile the data it needs to have
   * 3. Compile the token it needs to have
   * This box is used as initiation and ending of a funding process.
   * Keeps track of the number of funding
   * r4:
   * Tokens 0: LSNFT, 1: LST
   */
  lazy val serviceBoxScript: String =
    s"""{
       |  if (OUTPUTS(1).tokens(1)._2 == SELF.tokens(1)._2) {
       |    ownerPK
       |  }else{
       |    if(OUTPUTS(1).tokens(1)._2 == SELF.tokens(1)._2 + 1L){
       |      // Merging of services
       |
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            OUTPUTS(1).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |            OUTPUTS(1).R4[Long].get == SELF.R4[Long].get,
       |            OUTPUTS(1).propositionBytes == SELF.propositionBytes,
       |            OUTPUTS(1).tokens(0)._1 == lendServiceNFT,
       |            OUTPUTS(1).tokens(1)._1 == lendServiceToken,
       |            OUTPUTS(1).value >= SELF.value,
       |          )
       |        )
       |      )
       |    }else{
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            // whenever we interact with service box, it will only have 2 outputs.
       |            OUTPUTS.size == 2,
       |
       |            // service box
       |            OUTPUTS(1).propositionBytes == SELF.propositionBytes,
       |            OUTPUTS(1).tokens(0)._1 == lendServiceNFT,
       |            OUTPUTS(1).tokens(1)._1 == lendServiceToken,
       |            OUTPUTS(1).value >= SELF.value,
       |            OUTPUTS(1).tokens(1)._2 == SELF.tokens(1)._2 - 1,
       |        )
       |      }
       |    }
       |  }
       |}""".stripMargin
}
