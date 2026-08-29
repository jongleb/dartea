let after_prefix ~prefix written =
  if String.starts_with ~prefix written then
    let width = String.length prefix in
    Some (String.sub written width (String.length written - width))
  else None
