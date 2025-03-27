import React, { useState, useEffect } from 'react';
import styled, { keyframes } from 'styled-components';
import { FiX, FiCheck, FiAlertTriangle, FiInfo } from 'react-icons/fi';

// Tipi di toast
const TOAST_TYPES = {
  SUCCESS: 'success',
  ERROR: 'error',
  WARNING: 'warning',
  INFO: 'info'
};

// Animazioni
const slideIn = keyframes`
  from {
    transform: translateX(100%);
    opacity: 0;
  }
  to {
    transform: translateX(0);
    opacity: 1;
  }
`;

const slideOut = keyframes`
  from {
    transform: translateX(0);
    opacity: 1;
  }
  to {
    transform: translateX(100%);
    opacity: 0;
  }
`;

// Stili per i singoli toast
const ToastWrapper = styled.div`
  display: flex;
  align-items: center;
  background: ${props => {
    switch (props.type) {
      case TOAST_TYPES.SUCCESS:
        return '#4caf50';
      case TOAST_TYPES.ERROR:
        return '#f44336';
      case TOAST_TYPES.WARNING:
        return '#ff9800';
      case TOAST_TYPES.INFO:
      default:
        return '#2196f3';
    }
  }};
  color: white;
  padding: 12px 16px;
  border-radius: 4px;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
  margin-bottom: 10px;
  animation: ${props => props.visible ? slideIn : slideOut} 0.3s ease forwards;
`;

const IconContainer = styled.div`
  margin-right: 12px;
  display: flex;
  align-items: center;
  font-size: 20px;
`;

const MessageText = styled.div`
  flex: 1;
  font-size: 14px;
`;

const CloseButton = styled.button`
  background: transparent;
  border: none;
  color: white;
  cursor: pointer;
  display: flex;
  align-items: center;
  padding: 0;
  margin-left: 12px;
  opacity: 0.7;
  transition: opacity 0.2s;
  
  &:hover {
    opacity: 1;
  }
`;

// Componente Toast per messaggi di notifica
const Toast = ({ message, type = TOAST_TYPES.INFO, duration = 3000, onClose }) => {
  const [visible, setVisible] = useState(true);
  
  // Imposta l'icona in base al tipo
  const getIcon = () => {
    switch (type) {
      case TOAST_TYPES.SUCCESS:
        return <FiCheck />;
      case TOAST_TYPES.ERROR:
        return <FiAlertTriangle />;
      case TOAST_TYPES.WARNING:
        return <FiAlertTriangle />;
      case TOAST_TYPES.INFO:
      default:
        return <FiInfo />;
    }
  };
  
  // Chiude automaticamente il toast dopo la durata specificata
  useEffect(() => {
    const timer = setTimeout(() => {
      setVisible(false);
      
      // Aspetta che l'animazione sia terminata prima di chiamare onClose
      setTimeout(() => {
        if (onClose) onClose();
      }, 300);
    }, duration);
    
    return () => clearTimeout(timer);
  }, [duration, onClose]);
  
  // Gestisce la chiusura manuale
  const handleClose = () => {
    setVisible(false);
    
    // Aspetta che l'animazione sia terminata prima di chiamare onClose
    setTimeout(() => {
      if (onClose) onClose();
    }, 300);
  };
  
  return (
    <ToastWrapper visible={visible} type={type}>
      <IconContainer>{getIcon()}</IconContainer>
      <MessageText>{message}</MessageText>
      <CloseButton onClick={handleClose}>
        <FiX />
      </CloseButton>
    </ToastWrapper>
  );
};

// Esportazione delle utilità per gestire i toast a livello globale
export { TOAST_TYPES };
export default Toast;