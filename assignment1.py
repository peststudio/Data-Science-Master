# -*- coding: utf-8 -*-
import random
import string
def name_generator(first_name,second_name):
    len1 = len(first_name) // 2
    temp = first_name[0:len1]
    len2 = len(second_name) // 2
    temp = temp + second_name[len2:]
    return temp

def validate_age(age):
    if (age < 0 or age > 150):
        print('Age must be between 0 and 150')
        return False
    else:
        return True
        
def age_generator(first_age,second_age):
    temp_age = 0
    for i in first_age:
        temp_age += int(i)
    for i in second_age:
        temp_age += int(i)
    return temp_age
    
def get_birthday(year):
    month = random.randint(1,12)
    if month in [1,2,5,7,8,10,12]:
        day = random.randint(1,31)
    elif (month == 2):
        if (year % 4 == 0):
            day = random.randint(1,29)
        else:
            day = random.randint(1,28)
    else:
        day = random.randint(1,30)
    return str(day).zfill(2) + '/' + str(month).zfill(2) + '/' + str(year)
    
def get_ID(str_new_age):
    length = len(str_new_age)
    if length == 10:
        return str_new_age
    else:
        temp_digit = int(str_new_age[length - 1]) + int(str_new_age[length - 2])
        temp_digit = temp_digit % 10
        return get_ID(str_new_age + str(temp_digit))
        
def generate_output(full_name, new_birthday, new_ID):
    print(full_name + '\t' + new_ID + '\n')
    print(new_birthday + '\n')
    print('Authorised by ')

def get_age(which_one):
    age_valid = False
    while (age_valid == False): 
        try:
            age1 = int(input('What is the ' + which_one + ' age? '))
            age_valid = validate_age(age1)       
        except:
            print('Age must be numbers. Try again!') 
    return age1
    
def get_year():
    current_year = 0
    while(current_year == 0):
        try:
            current_year = int(input("What is the current year? "))
            if (current_year < 1900 or current_year > 2099):
                print('Current Year must be between 1900 and 2099')
                current_year = 0
        except:
            print("Year must be a number")
    return current_year
    
def generate_dict():
    list_letters = 'abcdefghijklmnopqrstuvwxyz'
    shift = random.randint(1,25)
    dict_letters = {}
    count = 0
    for i in list_letters:
        dict_letters[i] = list_letters[(count + shift) % 26]
        count += 1
    return shift, dict_letters

def encrypt_word(dict_letters,text):
    new_text = ''
    for i in text:
        if i == ' ':
            new_text = new_text + ' '
        else:
            try:
                new_text = new_text + dict_letters[i]
            except:
                new_text = new_text + dict_letters[(i.lower())].upper()
            
    return new_text
    
def main():   
    first_name1 = input('First name 1: ')
    first_name2 = input('First name 2: ')
    second_name1 = input('Second name 1: ')
    second_name2 = input('Second name 2: ')
    
    age1 = get_age('first')
    age2 = get_age('second')
    current_year = get_year()
    
            
    new_first_name = name_generator(first_name1,first_name2)
    new_second_name = name_generator(second_name1,second_name2)
    full_name = new_first_name + ' ' + new_second_name

    new_age = age_generator(str(age1),str(age2))
    birth_year = current_year - new_age
    new_birthday = get_birthday(birth_year)
    new_ID = get_ID(str(new_age))
    generate_output(full_name, new_birthday, new_ID)
    shift, dict_letters = generate_dict()
    encryted_name = encrypt_word(dict_letters, full_name)
    print(encryted_name, shift)
    
main()