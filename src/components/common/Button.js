import React from 'react';
import styled, { css } from 'styled-components';

// Componente Button riutilizzabile con diverse varianti
const Button = ({ 
  children, 
  variant = 'primary', 
  size = 'medium',
  icon,
  fullWidth = false,
  disabled = false,
  onClick,
  type = 'button',
  className,
  ...rest
}) => {
  return (
    <StyledButton
      type={type}
      variant={variant}
      size={size}
      fullWidth={fullWidth}
      disabled={disabled}
      onClick={onClick}
      className={className}
      {...rest}
    >
      {icon && <span className="icon">{icon}</span>}
      {children}
    </StyledButton>
  );
};

// Stili con styled-components
const StyledButton = styled.button`
  display: inline-flex;
  align-items: center;
  justify-content: center;
  border: none;
  border-radius: 4px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s ease;
  font-family: inherit;
  
  .icon {
    display: inline-flex;
    margin-right: ${props => props.children ? '8px' : '0'};
  }
  
  /* Varianti di colore */
  ${props => {
    switch (props.variant) {
      case 'primary':
        return css`
          background: #0066cc;
          color: white;
          
          &:hover {
            background: #0055aa;
          }
        `;
      case 'secondary':
        return css`
          background: #6c757d;
          color: white;
          
          &:hover {
            background: #5a6268;
          }
        `;
      case 'success':
        return css`
          background: #33aa33;
          color: white;
          
          &:hover {
            background: #2a882a;
          }
        `;
      case 'danger':
        return css`
          background: #ff3333;
          color: white;
          
          &:hover {
            background: #e60000;
          }
        `;
      case 'outline':
        return css`
          background: transparent;
          color: #0066cc;
          border: 1px solid #0066cc;
          
          &:hover {
            background: rgba(0, 102, 204, 0.1);
          }
        `;
      case 'text':
        return css`
          background: transparent;
          color: #0066cc;
          
          &:hover {
            background: rgba(0, 102, 204, 0.1);
          }
        `;
      default:
        return css`
          background: #0066cc;
          color: white;
          
          &:hover {
            background: #0055aa;
          }
        `;
    }
  }}
  
  /* Dimensioni */
  ${props => {
    switch (props.size) {
      case 'small':
        return css`
          padding: 6px 12px;
          font-size: 13px;
        `;
      case 'medium':
        return css`
          padding: 8px 16px;
          font-size: 15px;
        `;
      case 'large':
        return css`
          padding: 12px 20px;
          font-size: 16px;
        `;
      default:
        return css`
          padding: 8px 16px;
          font-size: 15px;
        `;
    }
  }}
  
  /* Larghezza */
  ${props => props.fullWidth && css`
    width: 100%;
  `}
  
  /* Disabilitato */
  &:disabled {
    opacity: 0.6;
    cursor: not-allowed;
    pointer-events: none;
  }
`;

export default Button;
