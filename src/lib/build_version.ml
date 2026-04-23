let current =
  match "%%VERSION%%" with
  | "" | "%%VERSION%%" -> "dev"
  | version -> version