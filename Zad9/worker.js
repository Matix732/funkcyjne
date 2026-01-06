import { parentPort, workerData } from 'worker_threads';

// workerData zawiera dane przekazane przy tworzeniu wątku
const { task, data } = workerData;

if (task === 'studentHours') {
    // Zadanie 4.0: MapReduce - Suma godzin
    // data = list of activities [{student: "Jan", hours: 2}, ...]

    const result = {};

    // Krok Map & Reduce w jednym przejściu (standard w JS dla prostych struktur)
    data.forEach(item => {
        if (!result[item.student]) {
            result[item.student] = 0;
        }
        result[item.student] += item.hours;
    });

    parentPort.postMessage(result);

} else if (task === 'bagOfWords') {
    // Zadanie 4.5: Bag of Words dla pojedynczego tekstu
    // data = "Ala ma kota" (string)

    const words = data.toLowerCase().split(/\W+/).filter(w => w.length > 0);
    const vector = {};

    words.forEach(word => {
        if (!vector[word]) {
            vector[word] = 0;
        }
        vector[word] += 1;
    });

    // Zwracamy wynik w formacie { "Ala ma kota": { ala: 1, ... } }
    // Aby zachować strukturę z poprzednich zadań
    const finalResult = {
        [data]: vector
    };

    parentPort.postMessage(finalResult);
}