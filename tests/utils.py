def check_is_valid(interpretations, formula):
    for i in interpretations:
        assert formula.truth(i)
