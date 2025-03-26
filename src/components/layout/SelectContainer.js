import styled from 'styled-components';

const SelectContainer = styled.div`
  margin-bottom: 20px;
  
  label {
    display: block;
    margin-bottom: 8px;
    font-weight: bold;
  }
  
  .select-wrapper {
    display: flex;
    gap: 10px;
    align-items: center;
    width: 100%;
    position: relative;
  }
  
  /* Assicurati che il select abbia larghezza sufficiente */
  select {
    min-width: 250px;
    max-width: 100%;
  }
`;

export default SelectContainer;
