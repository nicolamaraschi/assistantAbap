import React from 'react';
import styled from 'styled-components';

// Componente riutilizzabile per i gruppi di form
const FormGroup = ({ 
  label, 
  children, 
  tooltip, 
  inline = false,
  required = false 
}) => {
  return (
    <StyledFormGroup inline={inline}>
      {label && (
        <StyledLabel required={required}>
          {label}
          {tooltip && <Tooltip>{tooltip}</Tooltip>}
        </StyledLabel>
      )}
      <div className="control">
        {children}
      </div>
    </StyledFormGroup>
  );
};

// Stili con styled-components
const StyledFormGroup = styled.div`
  margin-bottom: 15px;
  display: ${props => props.inline ? 'flex' : 'block'};
  align-items: ${props => props.inline ? 'center' : 'stretch'};
  
  .control {
    ${props => props.inline ? 'flex: 1;' : ''}
  }
`;

const StyledLabel = styled.label`
  display: block;
  margin-bottom: 5px;
  font-weight: bold;
  color: #444;
  ${props => props.inline ? 'margin-right: 10px;' : ''}
  
  &::after {
    content: "${props => props.required ? ' *' : ''}";
    color: #e74c3c;
  }
`;

const Tooltip = styled.span`
  position: relative;
  margin-left: 8px;
  cursor: help;
  
  &::before {
    content: "?";
    display: inline-block;
    width: 16px;
    height: 16px;
    background: #0066cc;
    color: white;
    border-radius: 50%;
    text-align: center;
    line-height: 16px;
    font-size: 12px;
  }
  
  &:hover::after {
    content: attr(data-tooltip);
    position: absolute;
    top: -30px;
    left: 50%;
    transform: translateX(-50%);
    background: #333;
    color: white;
    padding: 5px 10px;
    border-radius: 4px;
    white-space: nowrap;
    z-index: 10;
  }
`;

export default FormGroup;
