const plot = (el) => {
  const collectionName = el.dataset.source;
  const groupName = el.dataset.groupName;
  const testName = el.dataset.testName;

  const data = window.dataset[collectionName];
  const groupData = data.groups.find((it) => it.name === groupName);
  const testData = groupData.tests.find((it) => it.name === testName);

  const results = testData.results;
  const values = results.map((r) => r.value);
  const timestamps = results.map((r) => r.timestamp);

  new Chart(el, {
    type: "line",
    data: {
      labels: timestamps.map((ts) => moment.unix(ts).toDate()), // Convert Unix seconds to Date objects
      datasets: [{ data: values, label: testName }],
    },
    options: {
      maintainAspectRatio: false,
      scales: {
        x: {
          type: "time",
          time: {
            displayFormats: {
              day: "MMM D, YYYY",
            },
          },
        },
        y: {
          beginAtZero: true,
        },
      },
    },
  });
};

const fetchData = (el) => {
  const collectionName = el.dataset.source;
  const datasource = `${collectionName}.json`;
  return fetch(datasource)
    .then((response) => response.json())
    .then((data) => {
      window.dataset = { ...window.dataset, [collectionName]: data };
    })
    .catch((error) => console.error("Error fetching data:", error));
};

const promises = Array.from(
  document.querySelectorAll(".caliper-collection"),
).map(fetchData);

Promise.all(promises).then(() =>
  document.querySelectorAll(".caliper-plot").forEach(plot),
);
