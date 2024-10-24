:- ['two.pl'].

% Cумма оценок
sum([], 0).
sum([H|T], Sum):-sum(T,R), Sum is H + R.

% Получить средний балл по каждой группе
average_group(Group, Avg) :-
    findall(Grade, grade(Group, _, _, Grade), Grades),
    sum(Grades, Sum), length(Grades, Len), Len > 0, Avg is Sum / Len.

% Получить таблицу групп и средний балл по каждой из групп
average_per_group(Averages) :-
    findall(Group, grade(Group, _, _, _), Groups),
    list_to_set(Groups, UniqueGroups),
    findall((Group, Avg), (member(Group, UniqueGroups), average_group(Group, Avg)), Averages).

% Для каждого предмета получить список студентов, не сдавших экзамен 
not_passed_students(Subject, Students) :-
    findall(Student, grade(_, Student, Subject, 2), StudentsList),
    list_to_set(StudentsList, Students).

% Найти количество не сдавших студентов в каждой из групп
count_not_passed_in_group(Group, Count) :- 
    findall(Student, grade(Group, Student, _, 2), NotPassedStudents),
    length(NotPassedStudents, Count).
