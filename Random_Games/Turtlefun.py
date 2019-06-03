import turtle

paper = turtle.Screen()
pen = turtle.Turtle()
person = input('Enter your initial side length: ')
triangle = input('Enter your initial side length: ')

for k in range(int(triangle)):
    for x in range(3):
        pen.forward(k*float(person))
    pen.left(120)
