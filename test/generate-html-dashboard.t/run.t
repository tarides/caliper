Generate an HTML page with the benchmark history.

  $ caliper generate-html
  $ ls output/
  chart.js
  index.html
  main.css
  $ cat output/index.html
  <!DOCTYPE html>
  <html lang="en">
  
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Caliper Dashboard</title>
    <link rel="stylesheet" href="main.css">
    <script src="chart.js"></script>
  </head>
  
  <body>
    <div id="container">
      <nav id="navbar">
        <button onclick="loadGraph('foo')">foo</button>
      </nav>
      <main id="content">
        <canvas id="myChart"></canvas>
      </main>
    </div>
    <script>
      const ctx = document.getElementById('myChart');
  
      new Chart(ctx, {
        type: 'bar',
        data: {
          labels: ['Red', 'Blue', 'Yellow', 'Green', 'Purple', 'Orange'],
          datasets: [{
            label: '# of Votes',
            data: [12, 19, 3, 5, 2, 3],
            borderWidth: 1
          }]
        },
        options: {
          scales: {
            y: {
              beginAtZero: true
            }
          }
        }
      });
    </script>
  </body>
  
  </html>
