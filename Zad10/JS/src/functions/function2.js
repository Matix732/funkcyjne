const { app } = require('@azure/functions');

//Funkcja 2 - Sortowanie listy
app.http('sortList', {
    methods: ['POST'],
    authLevel: 'anonymous',
    handler: async (request, context) => {
        try {
            const body = await request.json();
            const list = body.list;

            if (!Array.isArray(list)) {
                return { status: 400, body: JSON.stringify({ error: "Brak pola 'list' lub nie jest tablicą" }) };
            }

            const sortAsync = (arr) => {
                return new Promise((resolve) => {
                    const sorted = [...arr].sort((a, b) => a - b);
                    resolve(sorted);
                });
            };

            const result = await sortAsync(list);
            return { body: JSON.stringify({ sortedList: result }) };
        } catch (error) {
            return { status: 500, body: JSON.stringify({ error: error.message }) };
        }
    }
});