const { app } = require('@azure/functions');

//Funkcja 1 - Liczba pierwsza
app.http('isPrime', {
    methods: ['POST'],
    authLevel: 'anonymous',
    handler: async (request, context) => {
        try {
            const body = await request.json();
            const n = body.number;

            if (n === undefined) {
                return { status: 400, body: JSON.stringify({ error: "Brak pola 'number'" }) };
            }

            const checkPrime = (num) => {
                return new Promise((resolve) => {
                    if (num < 2) return resolve(false);
                    for (let i = 2; i <= Math.sqrt(num); i++) {
                        if (num % i === 0) return resolve(false);
                    }
                    resolve(true);
                });
            };

            const result = await checkPrime(n);
            return { body: JSON.stringify({ isPrime: result }) };
        } catch (error) {
            return { status: 500, body: JSON.stringify({ error: error.message }) };
        }
    }
});
