package features.lend.contracts

package object proxyContracts {
  /**
   * Test Refund Proxy Script
   */
  lazy val testProxyScript: String =
    s"""{
       |  val returnFee = {
       |    allOf(Coll(
       |      OUTPUTS(0).value >= (INPUTS(0).value - minFee),
       |      OUTPUTS(0).propositionBytes == fundersPk,
       |      HEIGHT > refundHeightThreshold
       |    ))
       |  }
       |
       |  sigmaProp(returnFee)
       |}
       |""".stripMargin

  /**
   * Lending Box Creation Proxy
   *
   * Ensure that it can only be used with service box to create lendBox
   *
   * Input Variables:
   * - refundHeightThreshold
   * - serviceNFT,
   * - serviceLendToken
   * - goal
   * - deadlineHeight
   * - interestRate
   * - repaymentHeightLength
   * - borrowerPk
   */
  lazy val createSingleLenderLendingBoxProxyScript: String =
    s"""{
       | // the amount of boxes as outputs, else return
       | if (OUTPUTS.size > 1) {
       |  val createLendConditions = {
       |    allOf(Coll(
       |      OUTPUTS(0).tokens(0)._1 == serviceNFT,
       |      OUTPUTS(1).tokens(0)._1 == serviceLendToken,
       |      OUTPUTS(1).R4[Coll[Long]].get(0) == goal,
       |      OUTPUTS(1).R4[Coll[Long]].get(1) == deadlineHeight,
       |      OUTPUTS(1).R4[Coll[Long]].get(2) == interestRate,
       |      OUTPUTS(1).R4[Coll[Long]].get(3) == repaymentHeightLength,
       |      OUTPUTS(1).R5[Coll[Coll[Byte]]].get(2) == borrowerPk
       |    ))
       |  }
       |
       |  sigmaProp(createLendConditions)
       | } else {
       |  val returnCreateLendFee = {
       |    allOf(Coll(
       |      OUTPUTS(0).value >= (INPUTS(0).value - minFee),
       |      OUTPUTS(0).propositionBytes == borrowerPk,
       |      HEIGHT > refundHeightThreshold,
       |    ))
       |  }
       |
       |  sigmaProp(returnCreateLendFee)
       | }
       |}
       |""".stripMargin

  /**
   * Funding box proxy script
   * When funding, the output is the funding box with increased value.
   *
   * Checks:
   * - The right box with right Id
   * - value == fully funded
   * - Lendbox via token
   *
   * Input Variables:
   * - minFee
   * - serviceLendToken
   * - boxIdToFund
   * - lenderPk
   */
  lazy val fundSingleLenderLendingBoxProxyScript: String =
    s"""{
       |  // ** Variable Declaration **
       |  val inputLendBox = INPUTS(0)
       |  val outputLendBox = OUTPUTS(0)
       |
       |  val deadlineHeight = inputLendBox.R4[Coll[Long]].get(1)
       |  val fundingGoal = inputLendBox.R4[Coll[Long]].get(0)
       |  val lendBoxId = inputLendBox.id
       |
       |
       |  // ** Fund **
       |  val deadlineReached = deadlineHeight < HEIGHT
       |  val boxIdCheck = boxIdToFund == lendBoxId
       |  if (boxIdCheck && !deadlineReached) {
       |
       |    val fundingValue = SELF.value
       |    val newFundedValue = inputLendBox.value + fundingValue - minFee
       |
       |    val outputLendBoxLenderPk = outputLendBox.R6[Coll[Byte]]
       |
       |    // -- Single Lender --
       |    //
       |    // Funds only happens once. Therefore must hit funding goal
       |
       |    val fundLendBox = {
       |      allOf(Coll(
       |        outputLendBox.value == newFundedValue,
       |        outputLendBox.value >= fundingGoal,
       |        outputLendBoxLenderPk == lenderPk,
       |        inputLendBox.tokens(0)._1 == serviceLendToken,
       |        boxIdToFund == lendBoxId
       |      ))
       |    }
       |
       |    sigmaProp(fundLendBox)
       |
       |  } else {
       |
       |    // ** REFUND **
       |
       |    val fundingGoalReached = INPUTS(0).value >= fundingGoal
       |    val returnFunding = {
       |      allOf(Coll(
       |        OUTPUTS(0).value <= (INPUTS(0).value - minFee),
       |        OUTPUTS(0).propositionBytes == lenderPk,
       |        (fundingGoalReached || deadlineReached)
       |      ))
       |    }
       |
       |    sigmaProp(returnFunding)
       |  }
       |}
       |""".stripMargin

  /**
   * When repaying the loan
   * Borrower will pay to a proxy script which will merge with Repayment box
   * to output a repayment box.
   *
   * For a proxy script, we find the minimal it needs for the contract to work. It
   * shouldnt be too complicated.
   *
   * For this one, we only compare it to the data within the repayment box output/input,
   * we just check if its the right box, and also check if the fund is right. Cause all we're
   * doing is transferring the value from this loan to the box.
   *
   * Therefore these are the things to check:
   * 1. Is it the right fund/repayment box via id
   * 2. the value of the repayment box after.
   * 3. RepaymentBox via token
   *
   * 2 -> outputRepaymentBox.value = inputRepaymentBox.value + proxyContract.value - minFee
   *
   * InputVariables:
   * - minFee
   * - boxIdToFund
   * - userAddress
   * - serviceRepaymentToken
   */
  lazy val repaySingleLenderLoanProxyScript: String =
    s"""
       |{
       |  // ** Variable Declaration **
       |  val inputRepaymentBox = INPUTS(0)
       |  val outputRepaymentBox = OUTPUTS(0)
       |  val repaymentBoxRepaymentDetails = inputRepaymentBox.R7[Coll[Long]]
       |  val repaymentBoxInfo = inputRepaymentBox.R5[Coll[Coll[Byte]]]
       |
       |  val repaymentGoal = repaymentBoxRepaymentDetails.get(1)
       |  val repaymentBoxId = inputRepaymentBox.id
       |
       |  val amountToRepay = SELF.value - minFee
       |  val amountRepaid = inputRepaymentBox.value
       |
       |  val amountRepaidOutput = amountToRepay + amountRepaid
       |
       |
       |  // ** Fund **
       |  val boxIdCheck = boxIdToFund == repaymentBoxId
       |  val repaymentGoalReached = repaymentGoal <= amountRepaid
       |  if (boxIdCheck && !repaymentGoalReached) {
       |
       |    // we check the id and if the value is correct.
       |    val repaymentCheck = {
       |      allOf(Coll(
       |        boxIdToFund == repaymentBoxId,
       |        inputRepaymentBox.tokens(0)._1 == serviceRepaymentToken,
       |        outputRepaymentBox.value == amountRepaidOutput
       |      ))
       |    }
       |
       |    sigmaProp(repaymentCheck)
       |
       |  } else {
       |
       |    // ** REFUND **
       |    //
       |    // else refund the amount repaid (this refund will not be transacted alongside an input of repayment box)
       |    // it has to be processed purely as a refund. Therefore it will be used as the only input
       |    val returnRepayment = {
       |      allOf(Coll(
       |        OUTPUTS(0).value <= (INPUTS(0).value - minFee),
       |        OUTPUTS(0).propositionBytes == userAddress, // user must receive the transaction back to his account
       |        (repaymentGoalReached)
       |      ))
       |    }
       |
       |    sigmaProp(returnRepayment)
       |  }
       |}
       |""".stripMargin
}
