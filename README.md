# Project: Music Composition

 This project is a decomposition of the song "I'm Good" 
 by Bebe Rexha and David Guetta. <br>
 Authors: Anh Thuc (Christina) Vu, John Miller, Luke Walters, Luke Caruso-Thompson. <br>
 Date: 2022-10-12. <br>

As we worked on decomposing the song, John chose to do the base composition, which takes in the notes making the song (B-flat, D#4, G4, C, F4, A4, D, E-flat) and mod to play the note G4 with dynamic and seq to play the whole song together. For some effects in the background, Luke chose to do the "slides" sound that will create some sounds like a piano shredding, which we added to the bridge part of the song. He used a map to map the range of notes from 40 and 77 for "slide right" and reverse the range for "slide left." After getting a list of notes, he applied seq to the list to play the notes in sequence. For the drums sound, I chose to do a function roll that takes three parameters m,n, and dur that will use mod percussion and duration as a fraction. We also worked together on figuring out how to use the function button-onclick to create a button that plays the song when a user clicks on it. Also, Oriel wanted to make some notes playing randomly in the background, so he created the function random-velocity that takes in two parameters that will generate a random number between the minimum and maximum numbers b using local bindings and cond. The same techniques are applied to the function randomize-dynamics that outputs a random number between 1 and 127. However, for the functions generate-random-dynamics and apply-random-dynamics, he used to map and fold-right to modify a list of random numbers and play random notes in the background. <br>

My role is to create some sound effects playing in the background, and I chose to do the drums. For the drums sound, I chose to do a function roll that takes three parameters m as a MIDI note, a number n, and a duration. This function will return the collection of notes that evenly spaced notes that fit the duration of the original note. Roll uses mod percussion and implements the duration which has a duration as a fraction in which the denominator is divided by two.  This function plays perfectly with the MIDI note of 35 (B major) and a quarter note. Also, to make the drums play harmonically with the base composition that John created, I used the function repeat to repeat the drum sounds seven times before using seq to space it out with rest qn. <br>

In general, I did not face any fundamental bug that made me fix the whole project or start the project over. However, while figuring out how to make a button that can play the song when the user clicks on the button, I encountered a bug that took me a while to figure out. The techniques we need to use when creating the button are button and button-onclick which takes in a button and a procedure. I was confused about how the procedure should be and just gave it multiple inputs. Initially, I called the function slam qn to test the button-onclick, and as a result, it gave me an error saying the function I used was an object, not a procedure. To fix this, I turned slam qn into a procedure by using lambda with no parameter. The error message was then cleared, and I also used the function play-composition to play slam qn. Finally, I and my group applied this technique to the whole song, so we were able to play the song by clicking on the button now. <br>


