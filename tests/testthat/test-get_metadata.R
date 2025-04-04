describe("get_metadata() creates a metadata object that can be used with clinsight", {
  it("Can recreate internal metadata using the metadata.xlsx template", {
    meta <- get_metadata(app_sys("metadata.xlsx"))
    expect_equal(meta, metadata)
  })
})
