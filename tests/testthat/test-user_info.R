library(wiztimebook)

test_that("Construct user object by new_user_info", {
  user <- new_user_info("user1@app.com", "/My Notes/", path.expand("~"))
  expect_equal(user$user_name, "user1@app.com")
  expect_equal(user$data_location, "/My Notes/")
  expect_equal(user$wiznote_home, path.expand("~"))
  expect_equal(user$index_db, file.path(path.expand("~"), "user1@app.com", "data", "index.db"))
  expect_equal(user$notes_folder, file.path(path.expand("~"), "user1@app.com", "data", "notes"))
})
