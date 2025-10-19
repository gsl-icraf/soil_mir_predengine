// =========================================================================
// Custom JavaScript for Shiny Dashboard
// =========================================================================

$(document).ready(function() {
  console.log('Custom JavaScript loaded successfully');
  
  // Initialize dashboard features
  initializeDashboard();
  initializeAnimations();
  initializeUtilities();
});

// =========================================================================
// Dashboard Initialization
// =========================================================================

function initializeDashboard() {
  // Add smooth transitions to navigation
  $('.btn-outline-primary').hover(
    function() {
      $(this).addClass('pulse');
    },
    function() {
      $(this).removeClass('pulse');
    }
  );
  
  // Add loading states to buttons (with proper file input exclusion)
  $('.btn').on('click', function(e) {
    const $btn = $(this);
    
    // Skip file input buttons - they have specific classes in Shiny
    if ($btn.hasClass('btn-default') || $btn.hasClass('input-group-addon') || 
        $btn.parent().hasClass('input-group-btn') || 
        $btn.closest('.shiny-input-container').find('input[type="file"]').length > 0) {
      return; // Let file inputs work normally
    }
    
    const originalText = $btn.html();
    
    // Add loading spinner
    $btn.html('<span class="loading-spinner"></span> Loading...');
    $btn.prop('disabled', true);
    
    // Restore original state after 2 seconds
    setTimeout(function() {
      $btn.html(originalText);
      $btn.prop('disabled', false);
    }, 2000);
  });
}

// =========================================================================
// Animation Functions
// =========================================================================

function initializeAnimations() {
  // Animate cards on hover
  $('.card').hover(
    function() {
      $(this).addClass('shadow-lg');
    },
    function() {
      $(this).removeClass('shadow-lg');
    }
  );
  
  // Add staggered animation to cards on page load
  $('.card').each(function(index) {
    $(this).css({
      'opacity': '0',
      'transform': 'translateY(30px)'
    });
    
    $(this).delay(index * 100).animate({
      'opacity': '1'
    }, 500).css('transform', 'translateY(0)');
  });
}

// =========================================================================
// Utility Functions
// =========================================================================

function initializeUtilities() {
  // Add tooltips to elements with data-bs-toggle="tooltip"
  $('[data-bs-toggle="tooltip"]').tooltip();
  
  // Add smooth scrolling to anchor links
  $('a[href^="#"]').on('click', function(event) {
    const target = $(this.hash);
    if (target.length) {
      event.preventDefault();
      $('html, body').animate({
        scrollTop: target.offset().top - 100
      }, 500);
    }
  });
  
  // Auto-hide alerts after 5 seconds
  $('.alert').delay(5000).fadeOut('slow');
}

// =========================================================================
// Shiny Integration Functions
// =========================================================================

// Function to show loading overlay
function showLoading(elementId) {
  const element = $('#' + elementId);
  const overlay = $('<div class="loading-overlay"><div class="loading-spinner"></div></div>');
  
  element.css('position', 'relative');
  element.append(overlay);
  
  overlay.css({
    'position': 'absolute',
    'top': '0',
    'left': '0',
    'width': '100%',
    'height': '100%',
    'background': 'rgba(255,255,255,0.8)',
    'display': 'flex',
    'align-items': 'center',
    'justify-content': 'center',
    'z-index': '1000'
  });
}

// Function to hide loading overlay
function hideLoading(elementId) {
  $('#' + elementId + ' .loading-overlay').fadeOut(300, function() {
    $(this).remove();
  });
}

// Function to show notification
function showNotification(message, type = 'info', duration = 3000) {
  const alertClass = `alert-${type}`;
  const notification = $(`
    <div class="alert ${alertClass} alert-dismissible fade show notification-toast" role="alert" style="
      position: fixed;
      top: 20px;
      right: 20px;
      z-index: 9999;
      min-width: 300px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    ">
      ${message}
      <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
    </div>
  `);
  
  $('body').append(notification);
  
  // Auto-hide after specified duration
  setTimeout(function() {
    notification.fadeOut(300, function() {
      $(this).remove();
    });
  }, duration);
}

// Function to animate value changes
function animateValue(elementId, start, end, duration = 1000) {
  const element = $('#' + elementId);
  const startTime = Date.now();
  
  const animate = function() {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / duration, 1);
    
    // Easing function (ease-out)
    const easeOut = 1 - Math.pow(1 - progress, 3);
    const current = start + (end - start) * easeOut;
    
    element.text(Math.round(current));
    
    if (progress < 1) {
      requestAnimationFrame(animate);
    }
  };
  
  animate();
}

// =========================================================================
// Chart and Visualization Helpers
// =========================================================================

// Function to resize charts when container changes
function resizeCharts() {
  // Trigger resize event for responsive charts
  $(window).trigger('resize');
  
  // Force Shiny plots to recalculate
  $('.shiny-plot-output').each(function() {
    $(this).trigger('resize');
  });
}

// Debounce function for performance
function debounce(func, wait, immediate) {
  let timeout;
  return function() {
    const context = this;
    const args = arguments;
    const later = function() {
      timeout = null;
      if (!immediate) func.apply(context, args);
    };
    const callNow = immediate && !timeout;
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
    if (callNow) func.apply(context, args);
  };
}

// =========================================================================
// Responsive Utilities
// =========================================================================

// Handle responsive behavior
$(window).on('resize', debounce(function() {
  // Recalculate layouts on window resize
  resizeCharts();
  
  // Adjust sidebar behavior on mobile
  if ($(window).width() < 768) {
    $('.bslib-sidebar').addClass('mobile-sidebar');
  } else {
    $('.bslib-sidebar').removeClass('mobile-sidebar');
  }
}, 250));

// =========================================================================
// Router Navigation Functions
// =========================================================================

// Update active navigation based on current route
function updateActiveNavigation() {
  const currentPath = window.location.hash.replace('#!', '') || '/';
  
  // Remove active class from all nav links
  $('.navbar-nav .nav-link').removeClass('active');
  
  // Add active class to current nav link
  $(`.navbar-nav .nav-link[href="#!/${currentPath === '/' ? '' : currentPath}"]`).addClass('active');
}

// Initialize router navigation
function initializeRouterNavigation() {
  // Update navigation on hash change
  $(window).on('hashchange', updateActiveNavigation);
  
  // Set initial active navigation
  updateActiveNavigation();
  
  // Add smooth transitions for route changes
  $('.router-page').addClass('fadeIn');
}

// Call router initialization when document is ready
$(document).ready(function() {
  setTimeout(initializeRouterNavigation, 100);
  initializeSpectralParticles();
});

// =========================================================================
// Spectral Particles Effect
// =========================================================================

function initializeSpectralParticles() {
  const particlesContainer = $('.spectral-particles');
  
  if (particlesContainer.length === 0) return;
  
  // Create particles
  function createParticle() {
    const particle = $('<div class="particle"></div>');
    
    // Random starting position
    const startX = Math.random() * window.innerWidth;
    const drift = (Math.random() - 0.5) * 200; // Random horizontal drift
    const duration = 8 + Math.random() * 4; // 8-12 seconds
    const delay = Math.random() * 2; // 0-2 second delay
    
    // Random color from golden petri palette
    const colors = [
      'rgba(218, 165, 32, 0.8)',  // Goldenrod
      'rgba(184, 134, 11, 0.7)',  // Dark goldenrod
      'rgba(205, 133, 63, 0.8)',  // Peru
      'rgba(160, 82, 45, 0.7)',   // Saddle brown
      'rgba(139, 0, 0, 0.6)'      // Dark red accent
    ];
    const color = colors[Math.floor(Math.random() * colors.length)];
    
    particle.css({
      left: startX + 'px',
      background: `radial-gradient(circle, ${color} 0%, transparent 100%)`,
      animationDuration: duration + 's',
      animationDelay: delay + 's',
      transform: `translateX(${drift}px)`
    });
    
    particlesContainer.append(particle);
    
    // Remove particle after animation
    setTimeout(() => {
      particle.remove();
    }, (duration + delay) * 1000);
  }
  
  // Create initial particles
  for (let i = 0; i < 15; i++) {
    setTimeout(() => createParticle(), i * 200);
  }
  
  // Continue creating particles
  setInterval(createParticle, 800);
}

// =========================================================================
// Glass Card Hover Effects
// =========================================================================

$(document).ready(function() {
  // Add tilt effect to glass card on mouse move
  $('.glass-card').on('mousemove', function(e) {
    const card = $(this);
    const rect = this.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    
    const centerX = rect.width / 2;
    const centerY = rect.height / 2;
    
    const rotateX = (y - centerY) / 10;
    const rotateY = (centerX - x) / 10;
    
    card.css({
      transform: `perspective(1000px) rotateX(${rotateX}deg) rotateY(${rotateY}deg) translateZ(20px)`
    });
  });
  
  $('.glass-card').on('mouseleave', function() {
    $(this).css({
      transform: 'perspective(1000px) rotateX(0deg) rotateY(0deg) translateZ(0px)'
    });
  });
  
  // Initialize scroll reveal for spectral cards
  initializeScrollReveal();
});

// =========================================================================
// Scroll Reveal Animation
// =========================================================================

function initializeScrollReveal() {
  // Set initial state for spectral cards
  $('.spectral-card').css({
    opacity: '0',
    transform: 'translateY(30px)'
  });
  
  // Animate cards when they come into view
  function revealCards() {
    $('.spectral-card').each(function(index) {
      const card = $(this);
      const rect = this.getBoundingClientRect();
      const windowHeight = $(window).height();
      
      if (rect.top < windowHeight - 100) {
        setTimeout(() => {
          card.css({
            opacity: '1',
            transform: 'translateY(0px)',
            transition: 'all 0.6s ease-out'
          });
        }, index * 150); // Stagger animation
      }
    });
  }
  
  // Initial reveal and on scroll
  setTimeout(revealCards, 500); // Delay to allow banner animation
  $(window).on('scroll', debounce(revealCards, 100));
}

// =========================================================================
// Keyboard Shortcuts
// =========================================================================

$(document).on('keydown', function(e) {
  // Navigation shortcuts
  if (e.ctrlKey || e.metaKey) {
    switch(e.which) {
      case 72: // Ctrl/Cmd + H for Home
        e.preventDefault();
        $('#nav_home').trigger('click');
        break;
      case 80: // Ctrl/Cmd + P for Prediction
        e.preventDefault();
        $('#nav_prediction').trigger('click');
        break;
    }
  }
  
  // ESC key to close modals or overlays
  if (e.which === 27) {
    $('.modal').modal('hide');
    $('.loading-overlay').fadeOut();
  }
});

// =========================================================================
// Data Export Utilities
// =========================================================================

// Function to export data as CSV
function exportToCSV(data, filename) {
  const csv = convertToCSV(data);
  downloadCSV(csv, filename);
}

function convertToCSV(objArray) {
  const array = typeof objArray !== 'object' ? JSON.parse(objArray) : objArray;
  let str = '';
  
  for (let i = 0; i < array.length; i++) {
    let line = '';
    for (let index in array[i]) {
      if (line !== '') line += ',';
      line += array[i][index];
    }
    str += line + '\r\n';
  }
  
  return str;
}

function downloadCSV(csv, filename) {
  const csvFile = new Blob([csv], { type: "text/csv" });
  const downloadLink = document.createElement("a");
  
  downloadLink.download = filename;
  downloadLink.href = window.URL.createObjectURL(csvFile);
  downloadLink.style.display = "none";
  
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
}

// =========================================================================
// Performance Monitoring
// =========================================================================

// Simple performance monitoring
function logPerformance(label) {
  if (console.time) {
    console.time(label);
  }
}

function endPerformanceLog(label) {
  if (console.timeEnd) {
    console.timeEnd(label);
  }
}

// Log page load performance
window.addEventListener('load', function() {
  endPerformanceLog('Page Load');
  console.log('Dashboard fully loaded and ready');
});