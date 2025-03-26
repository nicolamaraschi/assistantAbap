import React from 'react';
import styled from 'styled-components';
import Toast from './Toast';

// Componente per gestire centralmente tutti i toast dell'applicazione
const ToastManager = ({ toasts, position = 'top', onClose }) => {
  return (
    <ToastContainer position={position}>
      {toasts.map(toast => (
        <Toast
          key={toast.id}
          message={toast.message}
          type={toast.type}
          duration={toast.duration}
          onClose={() => onClose(toast.id)}
        />
      ))}
    </ToastContainer>
  );
};

// Stili per il container dei toast
const ToastContainer = styled.div`
  position: fixed;
  top: ${props => props.position === 'top' ? '20px' : 'auto'};
  bottom: ${props => props.position === 'bottom' ? '20px' : 'auto'};
  right: 20px;
  z-index: 1000;
  display: flex;
  flex-direction: column;
  gap: 10px;
`;

export default ToastManager;
