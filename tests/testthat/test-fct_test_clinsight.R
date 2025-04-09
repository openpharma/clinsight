describe("test_clinsight() works", {
  it("creates a temporary folder and temporary files for one-time use with run_app()", {
    local_mocked_bindings(
      run_app = function(...){
        expect_equal(Sys.getenv("GOLEM_CONFIG_ACTIVE"), "test")
      }
    )
    expect_no_error(test_clinsight())
  })
  it("errors with incorrect input", {
    expect_error(
      test_clinsight(clinsight_data = "incorrect_argument"),
      "Expecting a data frame"
      )
    expect_error(
      test_clinsight(meta_data = "incorrect_argument"),
      "Expecting a list"
      )
    expect_error(test_clinsight(clinsight_config = mtcars))
    expect_error(
      test_clinsight(clinsight_data = mtcars, clinsight_config = "default"),
      "The 'default' config should not be used with custom data"
    )
  })
})
