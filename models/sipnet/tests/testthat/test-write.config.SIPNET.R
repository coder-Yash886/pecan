test_that("write.config.SIPNET", {
  pth <- withr::local_tempdir()

  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = ""),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(
          met = list(path = "")
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  res <- write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(Amax = 5, AmaxFrac = 0.99, leafC = 47)),
    settings = s,
    run.id = "run1"
  )

  # (at least some) parameters updated
  param_result <- readLines(file.path(pth, "run", "run1", "sipnet.param"))
  expect_match(
    param_result,
    "aMax 10", # this is Amax * SLA,
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    param_result,
    "aMaxFrac 0.99", # raw template had 0.76
    fixed = TRUE,
    all = FALSE
  )
  # leaf C specific weight is leafC / SLA, with units converted to g C/m2 leaf
  expect_match(
    param_result,
    "leafCSpWt 235 ", # space at end to catch unit errors (eg fail on 23500)
    fixed = TRUE,
    all = FALSE
  )

})

test_that("write.config.SIPNET plantStorageNInit precedence in v2", {
  pth <- withr::local_tempdir()
  dir.create(file.path(pth, "run", "run_v2_default"), recursive = TRUE)

  s_v2 <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = "2.0"),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(met = list(path = "")),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  # 1. Default template value (5.0) is preserved without events file or special computation
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_default"
  )
  param_default <- readLines(file.path(pth, "run", "run_v2_default", "sipnet.param"))
  expect_match(param_default, "plantStorageNInit 5", fixed = TRUE, all = FALSE)

  # 2. Overriding via IC (including an intentional zero) is respected
  dir.create(file.path(pth, "run", "run_v2_ic_zero"), recursive = TRUE)
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_ic_zero",
    IC = list(plantStorageNInit = 0)
  )
  param_ic_zero <- readLines(file.path(pth, "run", "run_v2_ic_zero", "sipnet.param"))
  expect_match(param_ic_zero, "plantStorageNInit 0", fixed = TRUE, all = FALSE)

  # 3. Explicit IC value non-zero
  dir.create(file.path(pth, "run", "run_v2_ic_val"), recursive = TRUE)
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_ic_val",
    IC = list(plantStorageNInit = 12.5)
  )
  param_ic_val <- readLines(file.path(pth, "run", "run_v2_ic_val", "sipnet.param"))
  expect_match(param_ic_val, "plantStorageNInit 12.5", fixed = TRUE, all = FALSE)
})

test_that("write.config.SIPNET soilOrgNInit and litterOrgNInit precedence in v2", {
  pth <- withr::local_tempdir()

  s_v2 <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = "2.0"),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(met = list(path = "")),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  # 1. Template defaults (135, 14) preserved when no IC nitrogen is provided
  dir.create(file.path(pth, "run", "run_v2_n_default"), recursive = TRUE)
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_n_default"
  )
  param_default <- readLines(file.path(pth, "run", "run_v2_n_default", "sipnet.param"))
  expect_match(param_default, "soilOrgNInit 135",   fixed = TRUE, all = FALSE)
  expect_match(param_default, "litterOrgNInit 14",  fixed = TRUE, all = FALSE)

  # 2. Explicit zero (intentional bare / nitrogen-limited start) is respected
  dir.create(file.path(pth, "run", "run_v2_n_zero"), recursive = TRUE)
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_n_zero",
    IC = list(
      soil_organic_nitrogen_content   = 0,   # kg N m-2 -> 0 g N m-2
      litter_organic_nitrogen_content = 0
    )
  )
  param_zero <- readLines(file.path(pth, "run", "run_v2_n_zero", "sipnet.param"))
  expect_match(param_zero, "soilOrgNInit 0",   fixed = TRUE, all = FALSE)
  expect_match(param_zero, "litterOrgNInit 0", fixed = TRUE, all = FALSE)

  # 3. Explicit non-zero values are correctly unit-converted (kg -> g) and written
  dir.create(file.path(pth, "run", "run_v2_n_val"), recursive = TRUE)
  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list())),
    trait.values = list(pft1 = list()),
    settings = s_v2,
    run.id = "run_v2_n_val",
    IC = list(
      soil_organic_nitrogen_content   = 0.15,  # kg N m-2 -> 150 g N m-2
      litter_organic_nitrogen_content = 0.02   # kg N m-2 ->  20 g N m-2
    )
  )
  param_val <- readLines(file.path(pth, "run", "run_v2_n_val", "sipnet.param"))
  expect_match(param_val, "soilOrgNInit 150",  fixed = TRUE, all = FALSE)
  expect_match(param_val, "litterOrgNInit 20", fixed = TRUE, all = FALSE)
})


test_that("update_flag_lines", {
  txt <- c("!comment", "NITROG = 0", "GDD = 1")

  # existing lines updated, new lines added
  expect_equal(
    update_flag_lines(txt, c(GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
  expect_equal(
    update_flag_lines(txt, c(new_flag = 0, NITROG = "1")),
    c("!comment", "NITROG = 1", "GDD = 1", "new_flag = 0")
  )

  # empty flags return input
  expect_equal(txt, update_flag_lines(txt, c()))
  expect_equal(txt, update_flag_lines(txt, NULL))

  # unnamed arguments ignored
  expect_equal(txt, update_flag_lines(txt, c("unnamed")))
  expect_equal(
    update_flag_lines(txt, c(1, GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
})

test_that("leafNResorptionFrac trait reaches the v2 param file", {
  pth <- withr::local_tempdir()
  event_src_path <- file.path(pth, "events-a.in")
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
  writeLines("2025 1 irrig 0 1", con = event_src_path)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = "v2.2.0"),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(
          met = list(path = ""),
          events = list(path = event_src_path)
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(leafNResorptionFrac = 0.6)),
    settings = s,
    run.id = "run1"
  )

  param_result <- readLines(file.path(pth, "run", "run1", "sipnet.param"))
  expect_match(param_result, "leafNResorptionFrac 0.6", fixed = TRUE, all = FALSE)
})

