Generate an HTML page with the benchmark history.

  $ caliper generate-html
  $ ls html-output/
  _
  alpine.min.js
  index.html
  logo-with-name.svg
  main.css
  plot.js
  uPlot.iife.min.js
  uPlot.min.css
  $ cat html-output/index.html
  <!DOCTYPE html>
  <html lang="en" class="h-full bg-white">
  
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Caliper Dashboard</title>
    <link rel="stylesheet" href="main.css">
    <script src="alpine.min.js" defer></script>
  </head>
  
  <body class="h-full">
    <div class="bg-white">
      <div x-data="{ open: false }" @keydown.window.escape="open = false">
  
        <div x-show="open" class="relative z-50 lg:hidden"
          x-description="Off-canvas menu for mobile, show/hide based on off-canvas menu state." x-ref="dialog"
          aria-modal="true" style="display: none;">
  
          <div x-show="open" x-transition:enter="transition-opacity ease-linear duration-300"
            x-transition:enter-start="opacity-0" x-transition:enter-end="opacity-100"
            x-transition:leave="transition-opacity ease-linear duration-300" x-transition:leave-start="opacity-100"
            x-transition:leave-end="opacity-0" class="fixed inset-0 bg-gray-900/80"
            x-description="Off-canvas menu backdrop, show/hide based on off-canvas menu state." style="display: none;">
          </div>
  
          <div class="fixed inset-0 flex">
  
            <div x-show="open" x-transition:enter="transition ease-in-out duration-300 transform"
              x-transition:enter-start="-translate-x-full" x-transition:enter-end="translate-x-0"
              x-transition:leave="transition ease-in-out duration-300 transform" x-transition:leave-start="translate-x-0"
              x-transition:leave-end="-translate-x-full"
              x-description="Off-canvas menu, show/hide based on off-canvas menu state."
              class="relative mr-16 flex w-full max-w-xs flex-1" @click.away="open = false" style="display: none;">
  
              <div x-show="open" x-transition:enter="ease-in-out duration-300" x-transition:enter-start="opacity-0"
                x-transition:enter-end="opacity-100" x-transition:leave="ease-in-out duration-300"
                x-transition:leave-start="opacity-100" x-transition:leave-end="opacity-0"
                x-description="Close button, show/hide based on off-canvas menu state."
                class="absolute left-full top-0 flex w-16 justify-center pt-5" style="display: none;">
                <button type="button" class="-m-2.5 p-2.5" @click="open = false">
                  <span class="sr-only">Close sidebar</span>
                  <svg class="h-6 w-6 text-white" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor"
                    aria-hidden="true">
                    <path stroke-linecap="round" stroke-linejoin="round" d="M6 18L18 6M6 6l12 12"></path>
                  </svg>
                </button>
              </div>
  
              <div class="flex grow flex-col gap-y-5 overflow-y-auto bg-white px-6 pb-2">
                <div class="flex h-16 shrink-0 items-center">
                  <a href="#">
                    <img class="h-8 w-auto" src="logo-with-name.svg" alt="Caliper">
                  </a>
                </div>
                <nav class="flex flex-1 flex-col">
                  <ul role="list" class="-mx-2 space-y-1">
                    <li>
                      <a href="_/foo.html"
                        class="text-gray-700 hover:text-orange-600 hover:bg-gray-50 group flex gap-x-3 rounded-md p-2 text-sm leading-6 font-semibold"
                        x-state:on="Current" x-state:off="Default"
                        x-state-description="Current: &quot;bg-gray-50 text-orange-600&quot;, Default: &quot;text-gray-700 hover:text-orange-600 hover:bg-gray-50&quot;">
                        <span
                          class="flex h-6 w-6 shrink-0 items-center justify-center rounded-lg border text-[0.625rem] font-medium bg-white text-gray-400 border-gray-200 group-hover:border-orange-600 group-hover:text-orange-600">H</span>
                        <span class="truncate">foo</span>
                      </a>
                    </li>
                  </ul>
                </nav>
              </div>
            </div>
  
          </div>
        </div>
  
        <div class="hidden lg:fixed lg:inset-y-0 lg:z-50 lg:flex lg:w-72 lg:flex-col">
          <div class="flex grow flex-col gap-y-5 overflow-y-auto border-r border-gray-200 bg-white px-6">
            <div class="flex h-16 shrink-0 items-center">
              <a href="#">
                <img class="h-8 w-auto" src="logo-with-name.svg" alt="Caliper">
              </a>
            </div>
            <nav class="flex flex-1 flex-col">
              <ul role="list" class="-mx-2 space-y-1">
                <li>
                  <a href="_/foo.html"
                    class="text-gray-700 hover:text-orange-600 hover:bg-gray-50 group flex gap-x-3 rounded-md p-2 text-sm leading-6 font-semibold"
                    x-state-description="undefined: &quot;bg-gray-50 text-orange-600&quot;, undefined: &quot;text-gray-700 hover:text-orange-600 hover:bg-gray-50&quot;">
                    <span
                      class="flex h-6 w-6 shrink-0 items-center justify-center rounded-lg border text-[0.625rem] font-medium bg-white text-gray-400 border-gray-200 group-hover:border-orange-600 group-hover:text-orange-600">W</span>
                    <span class="truncate">foo</span>
                  </a>
                </li>
              </ul>
            </nav>
          </div>
        </div>
  
        <div class="sticky top-0 z-40 flex items-center gap-x-6 bg-white px-4 py-4 shadow-sm sm:px-6 lg:hidden">
          <button type="button" class="-m-2.5 p-2.5 text-gray-700 lg:hidden" @click="open = true">
            <span class="sr-only">Open sidebar</span>
            <svg class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor"
              aria-hidden="true">
              <path stroke-linecap="round" stroke-linejoin="round" d="M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5">
              </path>
            </svg>
          </button>
          <div class="flex-1 text-sm font-semibold leading-6 text-gray-900">Caliber</div>
        </div>
  
        <main class="py-10 lg:pl-72">
          <div class="px-4 sm:px-6 lg:px-8">
            <div class="mx-auto max-w-2xl text-center">
              <h2 class="mt-2 text-4xl font-bold tracking-tight text-gray-900 sm:text-6xl">Welcome to Caliper 👋</h2>
              <p class="mt-6 text-lg leading-8 text-gray-600">To get started, select a project from the list of projects
                on the left sidebar.</p>
            </div>
          </div>
        </main>
      </div>
    </div>
  </body>
  
  </html>
