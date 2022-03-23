#this chunk of code lines 2-23 is the example from documentation
x <- c(
  paste(
    "text from:",
    "http://www.webopedia.com/quick_ref/textmessageabbreviations_02.asp"
  ),
  "... understanding what different characters used in smiley faces mean:",
  "The close bracket represents a sideways smile )",
  "Add in the colon and you have sideways eyes :",
  "Put them together to make a smiley face :)",
  "Use the dash - to add a nose :-)",
  paste(
    "Change the colon to a semi-colon ;",
    "and you have a winking face ;) with a nose ;-)"
  ),
  paste(
    "Put a zero 0 (halo) on top and now you have a winking,",
    "smiling angel 0;) with a nose 0;-)"
  ),
  "Use the letter 8 in place of the colon for sunglasses 8-)",
  "Use the open bracket ( to turn the smile into a frown :-("
)
replace_emoticon(x)


#testing various emojis here using 3 examples from the tweets

test_text1 <- "Wishing the rain would go away! I need some sunshine and rainbows! :)"
replace_emoticon(test_text1)

test_text2 <- "Buuttt I've gotta walk on my own :( its freezing and it takes me like 20 minutes :'("
replace_emoticon(test_text2)

test_text3 <- "its so cold :( weather sucks"
replace_emoticon(test_text3)
