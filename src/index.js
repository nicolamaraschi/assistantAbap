import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import App from './App';
import MonkeyPatch from './components/common/MonkeyPatch';

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <MonkeyPatch />
    <App />
  </React.StrictMode>
);