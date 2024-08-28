describe("get_roles_from_config() works", {
  it("retrieves app roles, and errors with incorrect input", {
    # ensure the right config is used:
    withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
    expected_output <- c(
      "Administrator" = "admin", 
      "Medical Monitor" = "medical_monitor", 
      "Data Manager" = "data_manager"
    )
    expect_equal(
      get_roles_from_config(),
      expected_output
    )
    expect_error(get_roles_from_config(config_roles = data.frame()))
  })
})

describe("get_valid_roles() works", {
  it(
    "select roles out of available ones, in the order as described in the 
    config file", 
    {
      withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
      expected_output <- get_roles_from_config()[1:2]
      expect_equal(
        get_valid_roles(c("medical_monitor", "admin", "other")),
        expected_output
      )
    }
  )
  it("allows list-type input", {
    withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
    expected_output <- get_roles_from_config()[1:2]
    expect_equal(
      get_valid_roles(list("medical_monitor", "admin", "other")),
      expected_output
    )
  })
  it("errors with incorrect input", { 
    expect_error(get_valid_roles(as.Date("")))
    expect_error(get_valid_roles("", data.frame()))
  })
  it(
    "sanitizes role input by converting everything to lowercase, by 
     splitting up strings when commas are detected, and by removing white 
     spaces if applicable", 
    {
      withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
      expected_output <- get_roles_from_config()[c(1,3)]
      expect_equal(
        get_valid_roles(c("dATA_Manager,   AdMIN ", "other")),
        expected_output
      )
      expect_equal(
        get_valid_roles(list("dATA_Manager,   AdMIN ", "other")),
        expected_output
      )
    }
  )
  it("warns, but does not error, if input is empty", {
    expect_warning(
      get_valid_roles(NULL), 
      "No roles found. Is the active configuration correct?"
      )
    expect_equal(
      suppressWarnings(get_valid_roles(NULL)), 
      setNames(nm = character(0))
      )
  })
})
