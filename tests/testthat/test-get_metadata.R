describe("get_metadata() creates a metadata object that can be used with clinsight", {
  it("Can recreate internal metadata using the metadata.xlsx template", {
    meta <- get_metadata(app_sys("metadata.xlsx"))
    expect_equal(meta, metadata)
  })
  it("errors if an unsupported format (not xlsx) is provided", {
    expect_error(
      get_metadata("metadata.docx"),
      "only .xlsx files are supported"
    )
  })
  it("errors if expand_tab_items do not exist as an Excel tab", {
    expect_error(
      get_metadata(app_sys("metadata.xlsx"), expand_tab_items = "non-existent-tab"),
      "The following tab names in 'expand_tab_items' were not found"
    )
  })
  it("adds required metadata columns if they do not exist", {
    local_mocked_bindings(required_meta_cols = c("test_col"))
    expect_warning(
      meta <- get_metadata(app_sys("metadata.xlsx")),
      "Required column 'test_col' will be created "
    )
    expect_true("test_col" %in% names(meta$items_expanded))
  })
})
