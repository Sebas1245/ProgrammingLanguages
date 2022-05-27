% facts
suffers(peter, the_flu).
suffers(peter, hepatitis).
suffers(john, hepatitis).
suffers(mary, the_flu).
suffers(charles, intoxication).
symptom(fever, the_flu).
symptom(tiredness, hepatitis).
symptom(diarrhea, intoxication).
symptom(tiredness, the_flu).
suppresses(aspirin, fever).
suppresses(lomotil, diarrhea).

% rules
relieves(Drug, Disease) :- 
    symptom(Symptom, Disease), 
    suppresses(Drug, Symptom).
take_drug(Person, Drug) :- 
    suffers(Person, Disease),
    relieves(Drug, Disease).
