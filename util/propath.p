def var x as int.

repeat x = 1 to num-entries(propath):

  display entry(x,propath) format "x(70)".


end.

display search("wrep.p") skip.
display search("wrep.r") skip.
display search("wrepr.p") skip.
display search("wrepr.r").
