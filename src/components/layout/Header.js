import React from 'react';
import styled from 'styled-components';
import { useAbap } from '../../context/AbapContext';

const Header = () => {
  const { settings } = useAbap();
  
  return (
    <StyledHeader theme={settings.theme}>
      <h1>Generatore Avanzato di Costrutti ABAP</h1>
      <p className="subtitle">Crea codice ABAP pulito e ben formattato in pochi clic</p>
    </StyledHeader>
  );
};

const StyledHeader = styled.header`
  text-align: center;
  margin-bottom: 30px;
  padding: 30px 20px;
  background: ${props => props.theme === 'dark' 
    ? 'linear-gradient(135deg, #1a365d, #2a4365)' 
    : 'linear-gradient(135deg, #0066cc, #004080)'
  };
  color: white;
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
  
  h1 {
    margin: 0;
    font-size: 32px;
    font-weight: 700;
  }
  
  .subtitle {
    margin-top: 10px;
    font-style: italic;
    opacity: 0.8;
    font-size: 16px;
  }
`;

export default Header;
