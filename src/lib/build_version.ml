let current =  
  match "##Build_version##" with  
  | "" | "##Build_version##" -> "dev"
  | version -> version
