describe("test_clinsight() works", {
  it("creates a temporary folder and temporary files for one-time use with run_app()", {
    local_mocked_bindings(
      run_app = function(...){
        ### verify objects here
      }
    )
    expect_no_error(test_clinsight())
  })
})
