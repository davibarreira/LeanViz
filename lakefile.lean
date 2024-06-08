import Lake
open Lake DSL

package «LeanViz» where
  -- add package configuration options here

lean_lib «LeanViz» where
  -- add library configuration options here

@[default_target]
lean_exe «leanviz» where
  root := `Main
