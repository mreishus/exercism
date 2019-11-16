def convert(number):
    drops = []
    if number % 3 == 0:
        drops.append("Pling")
    if number % 5 == 0:
        drops.append("Plang")
    if number % 7 == 0:
        drops.append("Plong")
    if len(drops) == 0:
        return str(number)
    return "".join(drops)
