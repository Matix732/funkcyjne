import express from 'express';
import { Worker } from 'worker_threads';
import fs from 'fs/promises';

const app = express();
const PORT = 3000;

app.use(express.json());

// Funkcja pomocnicza do czytania JSON z pliku (symulacja zachowania ze Scali)
// Jeśli w body requestu nie ma danych, próbujemy czytać z pliku
const getData = async (req, defaultFilename) => {
    if (req.body && Object.keys(req.body).length > 0) {
        return req.body;
    }
    console.log(`Brak body, czytam z pliku: ${defaultFilename}`);
    try {
        const raw = await fs.readFile(defaultFilename, 'utf-8');
        return JSON.parse(raw);
    } catch (e) {
        throw new Error(`Nie znaleziono pliku ${defaultFilename} i brak danych w body.`);
    }
};

// Funkcja pomocnicza do uruchamiania Workera jako Promise
const runWorker = (taskName, taskData) => {
    return new Promise((resolve, reject) => {
        const worker = new Worker('./worker.js', {
            workerData: { task: taskName, data: taskData }
        });
        worker.on('message', resolve);
        worker.on('error', reject);
        worker.on('exit', (code) => {
            if (code !== 0) reject(new Error(`Worker stopped with exit code ${code}`));
        });
    });
};

// --- ZADANIE 9 ---

// 3.0: Liczba pierwsza (Promise)
app.post('/isPrime', async (req, res) => {
    try {
        const input = await getData(req, 'zad.3.0.json'); // oczekuje { number: Int }
        const num = input.number;

        const checkPrime = (n) => {
            return new Promise((resolve) => {
                if (n < 2) return resolve(false);
                for (let i = 2; i <= Math.sqrt(n); i++) {
                    if (n % i === 0) return resolve(false);
                }
                resolve(true);
            });
        };

        const result = await checkPrime(num);
        res.json({ isPrime: result });
    } catch (e) {
        res.status(500).json({ error: e.message });
    }
});

// 3.5: Sortowanie listy (Promise)
app.post('/sortList', async (req, res) => {
    try {
        const input = await getData(req, 'zad.3.5.json'); // oczekuje { list: [Int] }
        
        const sortAsync = (arr) => {
            return new Promise((resolve) => {
                // Kopiujemy tablicę, żeby nie mutować oryginału, sortujemy numerycznie
                const sorted = [...arr].sort((a, b) => a - b);
                resolve(sorted);
            });
        };

        const result = await sortAsync(input.list);
        res.json({ sortedList: result });
    } catch (e) {
        res.status(500).json({ error: e.message });
    }
});

// 4.0: MapReduce (Worker Threads) - Student Hours
app.post('/studentHours', async (req, res) => {
    try {
        const input = await getData(req, 'zad.4.0.json'); // { activities: [...] }
        
        // Uruchamiamy worker dla zadania 'studentHours'
        const result = await runWorker('studentHours', input.activities);
        
        res.json({ result });
    } catch (e) {
        res.status(500).json({ error: e.message });
    }
});

// 4.5: Bag of Words (Pula Workerów)
app.post('/bagOfWords', async (req, res) => {
    try {
        const input = await getData(req, 'zad.4.5.json'); // { sentences: [...] }
        const sentences = input.sentences;

        // Tworzymy "pulę" zadań - dla każdego zdania uruchamiamy osobny worker
        // Promise.all czeka, aż wszystkie wątki zakończą pracę
        const workerPromises = sentences.map(sentence => 
            runWorker('bagOfWords', sentence)
        );

        const resultsArray = await Promise.all(workerPromises);

        // Scalamy tablicę wyników w jeden obiekt
        const finalResult = Object.assign({}, ...resultsArray);

        res.json({ result: finalResult });
    } catch (e) {
        res.status(500).json({ error: e.message });
    }
});

// 5.0: Średnia ocen (Promise.all + map)
app.post('/studentAverage', async (req, res) => {
    try {
        const input = await getData(req, 'zad.5.0.json'); // { students: [], grades: [] }
        const { students, grades } = input;

        // Funkcja asynchroniczna licząca średnią dla jednego studenta
        const calculateAverageForStudent = async (student) => {
            // Symulacja asynchroniczności (np. szukanie w bazie)
            const studentGrades = grades.filter(g => g.studentId === student.id);
            
            if (studentGrades.length === 0) return null;

            const sum = studentGrades.reduce((acc, curr) => acc + curr.grade, 0);
            const avg = sum / studentGrades.length;
            
            return { name: student.name, average: avg };
        };

        // Wykorzystanie map i Promise.all zgodnie z poleceniem
        const promises = students.map(student => calculateAverageForStudent(student));
        const results = await Promise.all(promises);

        // Przekształcenie tablicy wyników w mapę (obiekt), usuwając nullowe (brak ocen)
        const responseMap = {};
        results.forEach(item => {
            if (item) {
                responseMap[item.name] = item.average;
            }
        });

        res.json({ result: responseMap });

    } catch (e) {
        res.status(500).json({ error: e.message });
    }
});

app.listen(PORT, () => {
    console.log(`Server is running on http://localhost:${PORT}`);
});