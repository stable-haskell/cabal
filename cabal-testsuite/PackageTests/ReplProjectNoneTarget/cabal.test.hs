import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  -- The following output is not what we want but is the current behaviour that
  -- refers to the fake package.
  log "checking repl command with no project and --ignore-project"
  ignored <- cabalWithStdin "repl" ["--ignore-project"] ""
  assertOutputContains "host fake-package-0 (lib)" ignored

  -- The following output is not what we want but is the current behaviour that
  -- refers to the fake package.
  log "checking repl command with no project and no project options"
  noOptions <- cabalWithStdin "repl" [] ""
  assertOutputContains "host fake-package-0 (lib)" noOptions

  log "checking repl command with a missing project"
  missing <- fails $ cabalWithStdin "repl" [ "--project-file=missing.project" ] ""
  assertOutputContains "The given project file 'missing.project' does not exist." missing

  return ()
