const { app } = require('@azure/functions');

//Funkcja 3 - Średnia ocen
app.http('studentAverage', {
    methods: ['POST'],
    authLevel: 'anonymous',
    handler: async (request, context) => {
        try {
            const body = await request.json();
            const { students, grades } = body;

            if (!students || !grades) {
                return { status: 400, body: JSON.stringify({ error: "Wymagane pola: students, grades" }) };
            }

            const calculateAverageForStudent = async (student) => {
                const studentGrades = grades.filter(g => g.studentId === student.id);
                if (studentGrades.length === 0) return null;
                const sum = studentGrades.reduce((acc, curr) => acc + curr.grade, 0);
                const avg = sum / studentGrades.length;
                return { name: student.name, average: avg };
            };

            const promises = students.map(student => calculateAverageForStudent(student));
            const results = await Promise.all(promises);

            const responseMap = {};
            results.forEach(item => {
                if (item) responseMap[item.name] = item.average;
            });

            return { body: JSON.stringify({ result: responseMap }) };

        } catch (error) {
            return { status: 500, body: JSON.stringify({ error: error.message }) };
        }
    }
});